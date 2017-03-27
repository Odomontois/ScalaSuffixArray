package odo.suffixarr;

import java.util.Arrays;
import java.util.BitSet;

abstract class SAISJBase {
    int n;
    int alphabetSize;
    BitSet typeMap;

    int[] summaryNames, summaryOffsets;
    int summarySize;
    int[] bucketSizes;
    int[] current;
    int[] summarySA;

    void init(int n, int alphabetSize) {
        this.n = n;
        this.alphabetSize = alphabetSize;
        bucketSizes = new int[alphabetSize];
        current = new int[n + 1];
        buildTypeMap();
        buildBucketSizes();
    }

    abstract boolean isLMS(int i);

    void showTypeMap() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i <= n; i++)
            if (typeMap.get(i)) sb.append('L');
            else if (isLMS(i)) sb.append('$');
            else sb.append('S');
    }

    abstract void buildTypeMap();

    abstract void buildBucketSizes();

    abstract void guessLMSSort();

    abstract void induceSortL();

    abstract void induceSortS();

    abstract boolean lmsSubsEqual(int i, int j);

    abstract void accurateLMSSort();

    int[] bucketHeads() {
        int[] res = new int[alphabetSize];
        int ptr = 1;
        for (int i = 0; i < alphabetSize; i++) {
            res[i] = ptr;
            ptr += bucketSizes[i];
        }
        return res;
    }

    int[] bucketTails() {
        int[] res = new int[alphabetSize];
        int ptr = 0;
        for (int i = 0; i < alphabetSize; i++) {
            ptr += bucketSizes[i];
            res[i] = ptr;
        }
        return res;
    }


    void start() {
        Arrays.fill(current, -1);
    }


    void printAt(int i) {
        StringBuilder sb = new StringBuilder();
        for (int k = 0; k < current.length; k++) {
            if (k > 0) sb.append(" ");
            if (i == k) sb.append("[").append(current[k]).append("]");
            else sb.append(current[k]);
        }
        System.out.println(sb);
    }


    void summarize() {
        int[] lmsNames = new int[n + 1];
        Arrays.fill(lmsNames, -1);
        int currentName = 0;
        lmsNames[current[0]] = currentName;
        int last = current[0];
        int count = 1;

        for (int i = 1; i <= n; i++) {
            int cur = current[i];
            if (!isLMS(cur)) continue;
            if (!lmsSubsEqual(cur, last)) currentName++;
            last = cur;
            lmsNames[cur] = currentName;
            count++;
        }

        summaryOffsets = new int[count];
        summaryNames = new int[count];
        summarySize = currentName + 1;

        int idx = 0;
        for (int i = 0; i <= n; i++) {
            int name = lmsNames[i];
            if (name == -1) continue;
            summaryOffsets[idx] = i;
            summaryNames[idx] = name;
            idx++;
        }
    }


    int[] suffixArray() {
        showTypeMap();
        guessLMSSort();
        induceSortL();
        induceSortS();
        summarize();
        buildSummarySA();
        accurateLMSSort();
        induceSortL();
        induceSortS();

        return current;
    }

    void buildSummarySA() {
        if (summaryNames.length == summarySize) {
            int m = summaryOffsets.length;
            summarySA = new int[m + 1];
            summarySA[0] = m;
            for (int i = 0; i < m; i++)
                summarySA[summaryNames[i] + 1] = i;
        } else {
            summarySA = new SAISJArray(summaryNames, summarySize).suffixArray();
        }
    }

}
