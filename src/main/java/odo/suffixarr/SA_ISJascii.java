package odo.suffixarr;

import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.BitSet;

public class SA_ISJascii {
    private byte[] seq;
    private int n;
    private BitSet typeMap;
    private final static int alphabetSize = 256;
    
    
    public SA_ISJascii(String seq) throws UnsupportedEncodingException {
        this.seq = seq.getBytes("ASCII");
        this.n = seq.length();
        bucketSizes = new int[alphabetSize];
        current = new int[n + 1];
        buildTypeMap();
        buildBucketSizes();
    }

    private int[] summaryNames, summaryOffsets;
    private int summarySize;
    private int[] bucketSizes;
    private int[] current;
    private int[] summarySA;

    private void buildTypeMap() {
        typeMap = new BitSet(n + 1);
        if (n > 0) typeMap.set(n - 1);
        for (int i = n - 2; i >= 0; i--) {
            int res = seq[i] - seq[i + 1];
            if (res > 0 || (res == 0 && typeMap.get(i + 1))) typeMap.set(i);
        }
    }

    private boolean isLMS(int i) {
        return i != 0 && !typeMap.get(i) && typeMap.get(i - 1);
    }

    private void showTypeMap() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i <= n; i++)
            if (typeMap.get(i)) sb.append('L');
            else if (isLMS(i)) sb.append('$');
            else sb.append('S');
    }

    private boolean lmsSubsEqual(int i, int j) {
        if (i == n || j == n || seq[i] != seq[j]) return false;
        while (true) {
            i++;
            j++;
            boolean isLMSi = isLMS(i);
            boolean isLMSj = isLMS(j);
            if (isLMSi && isLMSj) return true;
            if (isLMSi != isLMSj) return false;
            if (seq[i] != seq[j]) return false;
        }
    }

    private void buildBucketSizes() {
        for (int i = 0; i < n; i++) bucketSizes[seq[i]]++;
    }

    private int[] bucketHeads() {
        int[] res = new int[alphabetSize];
        int ptr = 1;
        for (int i = 0; i < alphabetSize; i++) {
            res[i] = ptr;
            ptr += bucketSizes[i];
        }
        return res;
    }

    private int[] bucketTails() {
        int[] res = new int[alphabetSize];
        int ptr = 0;
        for (int i = 0; i < alphabetSize; i++) {
            ptr += bucketSizes[i];
            res[i] = ptr;
        }
        return res;
    }

    private void start() {
        Arrays.fill(current, -1);
    }

    private void printAt(int i) {
        StringBuilder sb = new StringBuilder();
        for (int k = 0; k < current.length; k++) {
            if (k > 0) sb.append(" ");
            if (i == k) sb.append("[").append(current[k]).append("]");
            else sb.append(current[k]);
        }
        System.out.println(sb);
    }

    private void guessLMSSort() {
        start();
        int[] bt = bucketTails();
        for (int i = 0; i < n; i++) {
            if (!isLMS(i)) continue;
            int idx = seq[i];
            current[bt[idx]] = i;
            bt[idx]--;
        }
        current[0] = n;
    }

    private void induceSortL() {
        int[] bh = bucketHeads();

        for (int i = 0; i <= n; i++) {
            if (current[i] == -1) continue;
            int j = current[i] - 1;
            if (j < 0 || !typeMap.get(j)) continue;
            int idx = seq[j];
            current[bh[idx]] = j;
            bh[idx]++;
        }
    }

    private void induceSortS() {
        int[] bt = bucketTails();

        for (int i = n; i >= 0; i--) {
            int j = current[i] - 1;
            if (j < 0 || typeMap.get(j)) continue;
            int idx = seq[j];
            current[bt[idx]] = j;
            bt[idx]--;
        }
    }

    private void summarize() {
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

    private void accurateLMSSort() {
        int[] bt = bucketTails();
        start();
        for (int i = summarySA.length - 1; i > 1; i--) {
            int idx = summaryOffsets[summarySA[i]];
            int bIndex = seq[idx];
            current[bt[bIndex]] = idx;
            bt[bIndex]--;
        }
        current[0] = n;
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

    private void buildSummarySA() {

        if (summaryNames.length == summarySize) {
            int m = summaryOffsets.length;
            summarySA = new int[m + 1];
            summarySA[0] = m;
            for (int i = 0; i < m; i++)
                summarySA[summaryNames[i] + 1] = i;
        } else {
            summarySA = new SA_ISJ(new InducedSeq(), summarySize).suffixArray();
        }

    }

    class InducedSeq implements IntSeq {
        @Override
        public int size() {
            return summaryNames.length;
        }

        @Override
        public int get(int idx) {
            return summaryNames[idx];
        }
    }
}
