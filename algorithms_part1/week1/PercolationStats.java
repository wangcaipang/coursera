/*---------------------------------------------------------
 *  The implementation of PercolationStats.
 *---------------------------------------------------------*/

import edu.princeton.cs.algs4.StdRandom;
import edu.princeton.cs.algs4.StdStats;
import edu.princeton.cs.algs4.StdOut;


public class PercolationStats {
    private double[] result;
    private double meanResult;
    private double stddevResult;
    private int trialsResult;

    public PercolationStats(int n, int trials) {
        if (n < 0 || trials < 0) throw new IllegalArgumentException("illegal argumnent!");
        result = new double[trials];
        trialsResult = trials;
        for (int i = 0; i < trials; i++) {
            Percolation perco = new Percolation(n);
            while (!perco.percolates()) {
                int row = StdRandom.uniform(n) + 1;
                int col = StdRandom.uniform(n) + 1;
                perco.open(row, col);
            }
            result[i] = (double) perco.numberOfOpenSites() / (n * n);
        }
        meanResult = StdStats.mean(result);
        stddevResult = StdStats.stddev(result);
    }

    public double mean() {
        return meanResult;
    }

    public double stddev() {
        return stddevResult;
    }

    public double confidenceLo() {
        return meanResult - (1.96 * stddevResult / Math.sqrt(trialsResult));
    }

    public double confidenceHi() {
        return meanResult + (1.96 * stddevResult / Math.sqrt(trialsResult));
    }

    public static void main(String[] args) {
        PercolationStats percoStat = new PercolationStats(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
        StdOut.printf("mean = %f\n", percoStat.mean());
        StdOut.printf("stddev =  %f\n", percoStat.stddev());
        StdOut.printf("95%% confidence interval =  [%f, %f] \n", percoStat.confidenceLo(), percoStat.confidenceHi());
    }
}