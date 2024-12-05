package org.example;

import de.learnlib.oracle.SingleQueryOracle;
import net.automatalib.alphabet.Alphabet;
import net.automatalib.automaton.fsa.*;
import net.automatalib.automaton.graph.TransitionEdge;
import net.automatalib.graph.UniversalGraph;
import net.automatalib.word.Word;
import com.github.curiousoddman.rgxgen.RgxGen;

import java.io.IOException;
import java.util.*;
import java.util.regex.Pattern;

public class ProgramOracle implements SingleQueryOracle.SingleQueryOracleDFA<Character> {

    private final String program;
    private final RgxGen goldenGrammarGen;
    private final Pattern goldenGrammarPat;

    ProgramOracle(String program, String goldenGrammar) {
        this.program = program;
        this.goldenGrammarGen = RgxGen.parse(goldenGrammar);
        this.goldenGrammarPat = Pattern.compile(goldenGrammar);
    }

    public Boolean test(Word<Character> word) {
        try {
            String str = wordToString(word);
            Process p = Runtime.getRuntime().exec(new String[]{ "python3", "test.py", this.program, str});
            p.waitFor();
            return p.exitValue() == 0;
        } catch (IOException | InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    int numQueries = 0;

    @Override
    public Boolean answerQuery(Word<Character> prefix, Word<Character> suffix) {
        numQueries++;
        Word<Character> word = prefix.concat(suffix);
        return test(word);
    }

    public static String wordToString(Word<Character> word) {
        int[] codepoints = word.toIntArray((c) -> c);
        StringBuilder sb = new StringBuilder();
        for (int codepoint : codepoints) {
            sb.appendCodePoint(codepoint);
        }
        return sb.toString();
    }

    public double testPrecision(CompactDFA<Character> dfa) {
        Set<String> words = randomWords(dfa, 100, 1000);
        int ok = 0;
        for (String word : words) {
            //if (test(word)) {
            if (goldenGrammarPat.matcher(word).matches()) {
                ok++;
            }
        }
        return ok / (double)words.size();
    }

    public static Set<String> randomWords(CompactDFA<Character> dfa, int maxWordLength, int maxNumWords) {
        Random rand = new Random();
        Set<String> words = new HashSet<>();

        UniversalGraph<Integer, TransitionEdge<Character, Integer>, Boolean, TransitionEdge.Property<Character, Void>> graph = dfa.transitionGraphView();

        Queue<Map.Entry<Integer, String>> queue = new LinkedList<>();
        queue.add(new AbstractMap.SimpleEntry<>(dfa.getInitialState(), ""));

        while (!queue.isEmpty() && words.size() < maxNumWords) {
            Map.Entry<Integer, String> entry = queue.remove();
            Integer s = entry.getKey();
            String w = entry.getValue();
            if (dfa.isAccepting(s)) {
                words.add(w);
            }
            if (w.length() < maxWordLength) {
                Map<Integer, Set<Character>> ts = new HashMap<>();
                for (TransitionEdge<Character,Integer> e : graph.getOutgoingEdges(s)) {
                    Integer s1 = e.getTransition();
                    Set<Character> cs = ts.computeIfAbsent(s1, k -> new HashSet<>());
                    cs.add(e.getInput());
                }
                if (ts.keySet().size() == 1 && ts.keySet().contains(s) && !dfa.isAccepting(s)) {
                    // it's a trap!
                    continue;
                }
                for (Map.Entry<Integer, Set<Character>> t : ts.entrySet()) {
                    Integer s1 = t.getKey();
                    List<Character> chars = new ArrayList<>(t.getValue());
                    if (chars.size() > 1) {
                        Collections.shuffle(chars);
                        int numBranches = Math.min(chars.size()-1, rand.nextInt(3)+1);
                        chars = chars.subList(0, numBranches);
                    }
                    for (Character c : chars) {
                        String w1 = w + c;
                        queue.add(new AbstractMap.SimpleEntry<>(s1, w1));
                    }
                }
            }
        }

        return words;
    }

    public double testRecall(CompactDFA<Character> dfa) {
        Alphabet<Character> sigma = dfa.getInputAlphabet();
        int ok = 0;
        for (int i = 0; i < 1000; i++) {
            String golden = goldenGrammarGen.generate();
            Word<Character> w = Word.fromString(golden);
            if (w.stream().allMatch(sigma::containsSymbol) && dfa.accepts(w)) {
                ok++;
            };
        }
        return ok / 1000.0;
    }

    public Set<String> generatePositiveSamples(int maxNum) {
        Set<String> positiveSamples = new HashSet<>();
        for (int i = 0; i < maxNum; i++) {
            String golden = goldenGrammarGen.generate();
            positiveSamples.add(golden);
        }
        return positiveSamples;
    }

    public Set<String> generateNegativeSamples(int maxNum) {
        Set<String> positiveSamples = new HashSet<>();
        for (int i = 0; i < maxNum; i++) {
            String golden = goldenGrammarGen.generateNotMatching();
            positiveSamples.add(golden);
        }
        return positiveSamples;
    }

}