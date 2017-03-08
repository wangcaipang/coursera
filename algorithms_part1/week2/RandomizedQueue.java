import java.util.Iterator;

import edu.princeton.cs.algs4.StdRandom;

public class RandomizedQueue<Item> implements Iterable<Item> {
    private int N = 0;
    private Item[] cap = (Item[]) new Object[1];

    public RandomizedQueue() {

    }                 // construct an empty randomized queue

    public boolean isEmpty()                 // is the queue empty?
    {
        return N == 0;
    }

    public int size() {
        return N;
    }                        // return the number of items on the queue

    public void enqueue(Item item) {
        if (item == null) throw new java.lang.NullPointerException("");
        if (N == cap.length) {
            resize(cap.length * 2);
        }
        cap[N++] = item;
    }           // add the item

    public Item dequeue() {
        if (isEmpty()) throw new java.util.NoSuchElementException("");
        int dequeIndex = StdRandom.uniform(N);
        Item item = cap[dequeIndex];
        refactor(dequeIndex);
        if (N > 0 && N == cap.length / 4) {
            resize(cap.length / 2);
        }
        return item;
    }

    private void refactor(int dequeIndex) {
        for (int i = dequeIndex; i < N - 1; i++) {
            cap[i] = cap[i + 1];
        }
        N--;
    }

    // remove and return a random item
    private void resize(int size) {
        Item[] copy = (Item[]) new Object[size];
        for (int i = 0; i < N; i++) {
            copy[i] = cap[i];
        }
        cap = copy;
    }

    public Item sample() {
        if (isEmpty()) throw new java.util.NoSuchElementException("");
        return cap[StdRandom.uniform(N)];
    }                     // return (but do not remove) a random item

    public Iterator<Item> iterator() {
        return new ReverseArrayIterator();
    }        // return an independent iterator over items in random order

    private class ReverseArrayIterator implements Iterator<Item> {
        private int i = 0;
        private int[] indexArray = StdRandom.permutation(N);

        public boolean hasNext() {
            return i < N;
        }

        public Item next() {
            if (i >= N) throw new java.util.NoSuchElementException("");
            return cap[indexArray[i++]];
        }
    }

    public static void main(String[] args) {

    }   // unit testing (optional)
}