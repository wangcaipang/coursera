/*---------------------------------------------------------
 *  The implementation of Percolation.
 *---------------------------------------------------------*/

import edu.princeton.cs.algs4.WeightedQuickUnionUF;


public class Percolation {
    private WeightedQuickUnionUF preco;
    private int openSites;
    private int[] openList;
    private int percoSize;

    public Percolation(int n) {
        preco = new WeightedQuickUnionUF(n * n + 2);
        openList = new int[n * n];
        openSites = 0;
        percoSize = n;
    }

    public void open(int row, int col) {
        int pos = getPos(row, col);
        if (isOpen(row, col)) return;
        openSites++;
        openList[pos] = 1;
        int[] neighbors = getNeighbors(row, col);
        for (int i = 0; i < neighbors.length; i++) {
            if (neighbors[i] != -1) preco.union(pos, neighbors[i]);
        }
    }

    public boolean isOpen(int row, int col) {
        int pos = getPos(row, col);
        return openList[pos] == 1;
    }

    public boolean isFull(int row, int col) {
        int pos = getPos(row, col);
        return preco.connected(pos, percoSize * percoSize);

    }

    public int numberOfOpenSites() {
        return openSites;
    }

    public boolean percolates() {
        return preco.connected(percoSize * percoSize, percoSize * percoSize + 1);
    }

    private int getPos(int row, int col) {
        if (row < 1 || row > percoSize) throw new IndexOutOfBoundsException("row index out of bounds");
        if (col < 1 || col > percoSize) throw new IndexOutOfBoundsException("col index out of bounds");
        return (row - 1) * percoSize + col - 1;
    }

    private int[] getNeighbors(int row, int col) {
        int[] neighbors = new int[5];
        int lastIndex = 0;
        for (int i = 0; i < 5; i++) neighbors[i] = -1;
        if (row > 1 && isOpen(row - 1, col)) neighbors[lastIndex++] = getPos(row - 1, col);
        if (row < percoSize && isOpen(row + 1, col)) neighbors[lastIndex++] = getPos(row + 1, col);
        if (col > 1 && isOpen(row, col - 1)) neighbors[lastIndex++] = getPos(row, col - 1);
        if (col < percoSize && isOpen(row, col + 1)) neighbors[lastIndex++] = getPos(row, col + 1);
        if (row == 1) neighbors[lastIndex++] = percoSize * percoSize;
        if (row == percoSize) neighbors[lastIndex++] = percoSize * percoSize + 1;
        return neighbors;

    }

    public static void main(String[] args) {

    }
}