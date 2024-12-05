package org.example;

import net.automatalib.automaton.fsa.*;
import net.automatalib.automaton.graph.TransitionEdge;
import net.automatalib.graph.UniversalGraph;
import net.automatalib.serialization.dot.GraphDOT;
import org.json.JSONArray;
import org.json.JSONObject;
import com.github.curiousoddman.rgxgen.RgxGen;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

    public static void main(String[] args) throws IOException {
        System.setProperty("slf4j.internal.verbosity", "WARN");

        computePaniniRecall();
        System.out.println();
        runCGIDecodeTest();
        System.out.println();
        runPaniniTests();
    }

    public static void runCGIDecodeTest() throws IOException {
        File testFile = new File("../mimid/cgidecode.py");
        // these positive samples are from the Mimid benchmark
        List<String> positiveSamples = Arrays.asList(
                "Hello%2c+world%21",
                "Send+mail+to+me%40fuzzingbook.org",
                "name=Fred&class=101&math=2+%2B+2+%3D+4&",
                "name=Fred&status=good&status=happy&",
                "http://target/getdata.php?data=%3cscript%20src=%22http%3a%2f%2f",
                "www.badplace.com%2fnasty.js%22%3e%3c%2fscript%3e",
                "http://target/login.asp?userid=bob%27%3b%20update%20logintable%20set%20passwd%3d%270wn3d%27%3b--%00",
                "Colon%20%3A%20Hash%20%23%20Percent%20%25",
                "O%21nP%22BG%23JI%24Tw%25mJ%26bB%27xX%28zy%29Aj%2aZ",
                "E%2bNp%2cRP%2dVN%2eyV%2ftW%2AIJ%2BAe%2CkM%2DKf%2EB",
                "W%2FAo%3azF%3blw%3ctY%3dqy%3eLm%3fCS%3AyB%3BHq%3Ck",
                "y%3DZM%3EVH%3FRx%40gG%5bhh%5cjn%5dOD%5eDR%5fcu%5Bm",
                "b%5CJm%5Drl%5Ezn%5FKe%60hQ%7bBj%7chf%7dmB%7eyc%7Bp",
                "w%7CWd%7DCG%7Ec",
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
                "1234567890",
                "-./_~"
        );
        System.out.println("file,time_ms,precision,recall");
        runTest(testFile, new HashSet<>(positiveSamples),true, true);
    }

    public static void runPaniniTests() throws IOException {
        File testPath = new File("../eval_py");
        File[] testFiles = testPath.listFiles((dir, name) -> name.toLowerCase().endsWith(".py"));
        Arrays.sort(testFiles);

        System.out.println("file,time_ms,precision,recall");
        for (File testFile : testFiles) {
            runTest(testFile, null,true, true);
        }
    }

    public static void computePaniniRecall() {
        // these are tests for which Panini infers a subset of the golden grammar
        // we are interested in the recall these inferred subsets exhibit

        Map<String,Map.Entry<String,String>> tests = new HashMap<>();
        tests.put("250_(ab)S.py", new AbstractMap.SimpleEntry<>("(ab)*", "(ab)?"));
        tests.put("340_(aXb)S.py", new AbstractMap.SimpleEntry<>("(a.b)*", "(a.b)?"));
        tests.put("401_aS_bS.py", new AbstractMap.SimpleEntry<>("a*|b*", ""));
        tests.put("450_(a(^ab)b)S.py", new AbstractMap.SimpleEntry<>("(a[^ab]b)*", ""));
        tests.put("470_(0_01)S.py", new AbstractMap.SimpleEntry<>("(0|01)*", ""));

        System.out.println("file,panini_recall");
        for (Map.Entry<String,Map.Entry<String,String>> test : tests.entrySet()) {
            System.out.printf("%s,", test.getKey());
            String golden = test.getValue().getKey();
            String inferred = test.getValue().getValue();
            System.out.printf(Locale.US, "%.4f\n", testRecallRegex(golden, inferred));
        }
    }

    public static void runTest(File testFile, Set<String> knownPositiveSamples, boolean saveDotFile, boolean saveGrammar) throws IOException {
        System.out.print(testFile.getName()+",");

        String outFile = testFile.getAbsolutePath() + ".out";
        String goldenGrammar = extractGoldenGrammar(outFile);
        ProgramOracle oracle = new ProgramOracle(testFile.getAbsolutePath(), goldenGrammar);

        ExecutorService executor = Executors.newSingleThreadExecutor();
        final Future<CompactDFA<Character>> handler = executor.submit(new Callable<>() {
            @Override
            public CompactDFA<Character> call() throws Exception {
                if (knownPositiveSamples != null) {
                    return Learner.learnWithSamples(oracle, knownPositiveSamples, true);
                } else {
                    return Learner.learn(oracle, 20, true);
                }
            }
        });

        long start = System.currentTimeMillis();
        CompactDFA<Character> result = null;
        try {
            result = handler.get(Duration.ofMinutes(5).toMillis(), TimeUnit.MILLISECONDS);
        } catch (Exception e) {
            handler.cancel(true);
        }
        executor.shutdownNow();
        long end = System.currentTimeMillis();
        long timeElapsed = end - start;
        System.out.print(timeElapsed+",");

        if (result == null) {
            System.out.print("0.0000,0.0000\n");
            return;
        }

        if (saveDotFile) {
            String dfa_outfile = testFile.getAbsolutePath() + ".dfa.dot";
            StringBuilder b = new StringBuilder();
            GraphDOT.write(result, result.getInputAlphabet(), b);
            try (PrintWriter out = new PrintWriter(dfa_outfile)) {
                out.println(b.toString());
            }
        }

        if (saveGrammar) {
            String gram_outfile = testFile.getAbsolutePath() + ".grammar.json";
            JSONObject grammar = dfaToMimidGrammar(result);
            try (PrintWriter out = new PrintWriter(gram_outfile)) {
                out.println(grammar.toString());
            }
        }

        double precision = oracle.testPrecision(result);
        System.out.printf(Locale.US, "%.4f,", precision);

        double recall = oracle.testRecall(result);
        System.out.printf(Locale.US, "%.4f", recall);

        System.out.print("\n");
    }

    static String extractGoldenGrammar(String outFilename) throws IOException {
        String out = Files.readString(Paths.get(outFilename));
        Pattern pat = Pattern.compile(".*∈ (.*)}.*|.*= \"(.*)\"}.*|.*: \\(s:𝕊\\) → .*");
        Matcher m = pat.matcher(out);
        m.find();
        if (m.group(1) != null) {
            return m.group(1);
        } else if (m.group(2) != null) {
            return m.group(2);
        } else {
            return ".*";
        }
    }

    public static JSONObject dfaToMimidGrammar(CompactDFA<Character> dfa) {
        UniversalGraph<Integer, TransitionEdge<Character, Integer>, Boolean, TransitionEdge.Property<Character, Void>> graph = dfa.transitionGraphView();
        JSONObject grammar = new JSONObject();
        List<String> start = new ArrayList<>();
        start.add("<s" + dfa.getInitialState().toString() + ">");
        if (dfa.isAccepting(dfa.getInitialState())) {
            start.add("");
        }
        grammar.put("<START>", new JSONArray(start));
        int i = 0;
        Map<Set<Character>, String> terminals = new HashMap<>();
        for (Integer state : dfa.getStates()) {
            Map<Integer, Set<Character>> charsForSuccessor = new HashMap<>();
            for (TransitionEdge<Character,Integer> trans : graph.getOutgoingEdges(state)) {
                Integer next = trans.getTransition();
                Set<Character> chars = charsForSuccessor.computeIfAbsent(next, k -> new HashSet<>());
                chars.add(trans.getInput());
            }
            JSONArray p = new JSONArray();
            for (Map.Entry<Integer, Set<Character>> entry : charsForSuccessor.entrySet()) {
                Integer targetState = entry.getKey();
                if (entry.getValue().size() == 1) {
                    p.put(entry.getValue().toArray()[0].toString() + "<s" + targetState.toString() + ">");
                    if (dfa.isAccepting(targetState)) {
                        p.put(entry.getValue().toArray()[0].toString());
                    }
                } else {
                    String t = terminals.get(entry.getValue());
                    if (t == null) {
                        t = "<t" + i++ + ">";
                        terminals.put(entry.getValue(), t);
                        grammar.put(t, entry.getValue());
                    }
                    p.put(t + "<s" + targetState.toString() + ">");
                    if (dfa.isAccepting(targetState)) {
                        p.put(t);
                    }
                }
            }
            grammar.put("<s" + state.toString() + ">", p);
        }
        return grammar;
    }

    public static double testRecallRegex(String goldenRegex, String inferredRegex) {
        RgxGen goldenGrammarGen = RgxGen.parse(goldenRegex);
        Pattern inferredGrammarPat = Pattern.compile(inferredRegex);

        int ok = 0;
        for (int i = 0; i < 1000; i++) {
            String golden = goldenGrammarGen.generate();
            if (inferredGrammarPat.matcher(golden).matches()) {
                ok++;
            };
        }
        return ok / 1000.0;
    }

}

