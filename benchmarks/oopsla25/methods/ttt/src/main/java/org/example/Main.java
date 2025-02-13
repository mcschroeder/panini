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

        String program = args[0];
        String goldenFile = args[1];
        Integer numPosSamples = Integer.valueOf(args[2]);
        String outputFile = args[3];

        String goldenGrammar = Files.readString(Paths.get(goldenFile));
        ProgramOracle oracle = new ProgramOracle(program, goldenGrammar);
        Set<String> positiveSamples = oracle.generatePositiveSamples(numPosSamples);
        CompactDFA<Character> result = learn(oracle, positiveSamples);
        
        if (result == null) {
            System.exit(1);
        } else {
            JSONObject grammar = dfaToMimidGrammar(result);        
            try (PrintWriter out = new PrintWriter(outputFile)) {
                out.println(grammar.toString());
            }
        }
    }

    private static CompactDFA<Character> learn(ProgramOracle oracle, Set<String> positiveSamples) {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        final Future<CompactDFA<Character>> handler = executor.submit(new Callable<>() {
            @Override
            public CompactDFA<Character> call() throws Exception {
                return Learner.learnWithSamples(oracle, positiveSamples, true);
            }
        });
        CompactDFA<Character> result = null;
        try {
            result = handler.get(Duration.ofMinutes(5).toMillis(), TimeUnit.MILLISECONDS);
        } catch (Exception e) {
            handler.cancel(true);
        }
        executor.shutdownNow();
        return result;
    }

    public static JSONObject dfaToMimidGrammar(CompactDFA<Character> dfa) {
        UniversalGraph<Integer, TransitionEdge<Character, Integer>, Boolean, TransitionEdge.Property<Character, Void>> graph = dfa.transitionGraphView();
        JSONObject grammar = new JSONObject();
        List<String> start = new ArrayList<>();
        start.add("<s" + dfa.getInitialState().toString() + ">");
        grammar.put("<start>", new JSONArray(start));
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
            if (state == dfa.getInitialState() && dfa.isAccepting(state)) {
                p.put("");
            }
            grammar.put("<s" + state.toString() + ">", p);
        }
        return grammar;
    }

}

