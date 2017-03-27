package odo.suffixarr;

import java.util.BitSet;

public class SAISJ extends SAISJBase {
    private IntSeq seq;


    public SAISJ(IntSeq seq, int alphabetSize) {
        this.seq = seq;
        this.n = seq.size();
        this.alphabetSize = alphabetSize;
        bucketSizes = new int[alphabetSize];
        current = new int[n + 1];
        buildTypeMap();
        buildBucketSizes();
    }

    protected boolean isLMS(int i) {
        return i != 0 && !typeMap.get(i) && typeMap.get(i - 1);
    }

    void buildTypeMap() {
        typeMap = new BitSet(n + 1);
        if (n > 0) typeMap.set(n - 1);
        for (int i = n - 2; i >= 0; i--) {
            int res = seq.get(i) - seq.get(i + 1);
            if (res > 0 || (res == 0 && typeMap.get(i + 1))) typeMap.set(i);
        }
    }

    boolean lmsSubsEqual(int i, int j) {
        if (i == n || j == n || seq.get(i) != seq.get(j)) return false;
        while (true) {
            i++;
            j++;
            boolean isLMSi = isLMS(i);
            boolean isLMSj = isLMS(j);
            if (isLMSi && isLMSj) return true;
            if (isLMSi != isLMSj) return false;
            if (seq.get(i) != seq.get(j)) return false;
        }
    }

    void buildBucketSizes() {
        for (int i = 0; i < n; i++) bucketSizes[seq.get(i)]++;
    }

    void guessLMSSort() {
        start();
        int[] bt = bucketTails();
        for (int i = 0; i < n; i++) {
            if (!isLMS(i)) continue;
            int idx = seq.get(i);
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
            int idx = seq.get(j);
            current[bh[idx]] = j;
            bh[idx]++;
        }
    }

    void induceSortS() {
        int[] bt = bucketTails();

        for (int i = n; i >= 0; i--) {
            int j = current[i] - 1;
            if (j < 0 || typeMap.get(j)) continue;
            int idx = seq.get(j);
            current[bt[idx]] = j;
            bt[idx]--;
        }
    }


    void accurateLMSSort() {
        int[] bt = bucketTails();
        start();
        for (int i = summarySA.length - 1; i > 1; i--) {
            int idx = summaryOffsets[summarySA[i]];
            int bIndex = seq.get(idx);
            current[bt[bIndex]] = idx;
            bt[bIndex]--;
        }
        current[0] = n;
    }
}
