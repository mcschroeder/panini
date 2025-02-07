package org.example;

import de.learnlib.oracle.SingleQueryOracle;
import net.automatalib.alphabet.Alphabet;
import net.automatalib.automaton.fsa.*;
import net.automatalib.automaton.graph.TransitionEdge;
import net.automatalib.graph.UniversalGraph;
import net.automatalib.word.Word;
import com.github.curiousoddman.rgxgen.RgxGen;

import java.io.*;
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
            ProcessBuilder pb = new ProcessBuilder("python3", this.program);
            Process p = pb.start();
            OutputStream os = p.getOutputStream();
            BufferedWriter w = new BufferedWriter(new OutputStreamWriter(os));
            String str = wordToString(word);
            w.write(str);
            w.flush();
            w.close();
            int exitCode = p.waitFor();
            return exitCode == 0;
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

    public Set<String> generatePositiveSamples(int maxNum) {
        Set<String> positiveSamples = new HashSet<>();
        int i = 0;
        while (positiveSamples.size() < maxNum && i < 5*maxNum) {
            String golden = goldenGrammarGen.generate();
            positiveSamples.add(golden);
            i++;
        }
        return positiveSamples;
    }

    public Set<String> generateNegativeSamples(int maxNum) {
        Set<String> negativeSamples = new HashSet<>();
        int i = 0;
        while (negativeSamples.size() < maxNum && i < 5*maxNum) {
            String golden = goldenGrammarGen.generateNotMatching();
            negativeSamples.add(golden);
            i++;
        }
        return negativeSamples;
    }

}