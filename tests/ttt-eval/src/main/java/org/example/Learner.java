package org.example;

import de.learnlib.acex.AcexAnalyzers;
import de.learnlib.algorithm.ttt.dfa.TTTLearnerDFA;
import de.learnlib.query.DefaultQuery;
import net.automatalib.alphabet.Alphabet;
import net.automatalib.alphabet.Alphabets;
import net.automatalib.automaton.fsa.*;
import net.automatalib.util.automaton.fsa.DFAs;
import net.automatalib.word.Word;
import de.learnlib.oracle.equivalence.SampleSetEQOracle;

import java.util.*;

public class Learner {
    public static CompactDFA<Character> learn(ProgramOracle oracle, int numPosSamples, boolean alphabetFromSamples) {
        Set<String> positiveSamples = oracle.generatePositiveSamples(numPosSamples);
        return learnWithSamples(oracle, positiveSamples, alphabetFromSamples);
    }
    public static CompactDFA<Character> learnWithSamples(ProgramOracle oracle, Set<String> positiveSamples, boolean alphabetFromSamples) {
        Alphabet<Character> sigma;
        if (alphabetFromSamples) {
            Set<Character> symbols = new HashSet<>();
            for (String sample : positiveSamples) {
                for (char c : sample.toCharArray()) {
                    symbols.add(c);
                }
            }
            sigma = Alphabets.fromCollection(symbols);
        } else {
            sigma = Alphabets.characters((char)32, (char)126);
        }

        TTTLearnerDFA<Character> learner = new TTTLearnerDFA<>(sigma, oracle, AcexAnalyzers.LINEAR_FWD);

        SampleSetEQOracle<Character, Boolean> eqOracle = new SampleSetEQOracle<>(true);
        for (String sample : positiveSamples) {
            eqOracle.add(Word.fromString(sample), true);
        }

        DefaultQuery<Character, Boolean> counterexample = null;
        do {
            if (counterexample == null) {
                learner.startLearning();
            } else {
                learner.refineHypothesis(counterexample);
            }
            counterexample = eqOracle.findCounterExample(learner.getHypothesisModel(), sigma);
        } while (counterexample != null);


        return DFAs.minimize(learner.getHypothesisDS());
    }
}