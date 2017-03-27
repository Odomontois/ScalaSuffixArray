package odo.suffixarr;

import java.util.BitSet;

public class SAISJArray extends SAISJBase {
    private int[] seq;

    public SAISJArray(int [] seq, int alphabetSize){
        this.seq = seq;
        init(seq.length, alphabetSize);
    }

    protected void buildTypeMap() {
        typeMap = new BitSet(n + 1);
        if (n > 0) typeMap.set(n - 1);
        for (int i = n - 2; i >= 0; i--) {
            int res = seq[i] - seq[i + 1];
            if (res > 0 || (res == 0 && typeMap.get(i + 1))) typeMap.set(i);
        }
    }

    boolean isLMS(int i) {
        return i != 0 && !typeMap.get(i) && typeMap.get(i - 1);
    }

    boolean lmsSubsEqual(int i, int j) {
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

    protected void buildBucketSizes() {
        for (int i = 0; i < n; i++) bucketSizes[seq[i]]++;
    }

    void guessLMSSort() {
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

    void induceSortL() {
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

    void induceSortS() {
        int[] bt = bucketTails();

        for (int i = n; i >= 0; i--) {
            int j = current[i] - 1;
            if (j < 0 || typeMap.get(j)) continue;
            int idx = seq[j];
            current[bt[idx]] = j;
            bt[idx]--;
        }
    }

    void accurateLMSSort() {
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
}
