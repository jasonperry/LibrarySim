using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LibraryCode
{
    public class ShelvingException : Exception
    {
        public ShelvingException(string message) : base(message)
        { }
    }

    /** A one-sided bookshelf with a classification system */
    // It seems to work whether I put the template on IComparable or not?!
    public class Bookshelf<CN> where CN : CallNumber<CN>
    {
        public int numShelves { get; }
        /** Dimensions of shelves in this bookcase in mm. *
          * can have different height for every shelf. */
        int shelfLength, shelfDepth;
        /** Total linear mm of storage in bookcase. */
        public int totalSpace { get; }
        /** Free space remaining in bookcase. */
        public int freeSpace { get; protected set; } = 0;  // no shelves at first
        /** First and last call numbers stored on this shelf. */
        CN startCallNumber, endCallNumber;

        public class BookPlace
        {
            public Book theBook;
            public bool onShelf;
            public int pos; // millimeters from left.

            public BookPlace(Book book, int pos)
            {
                this.theBook = book;
                this.pos = pos;
                this.onShelf = true; // default when creating
            }
        }
        /** Seems that 'struct' is for when you want value semantics.   *
          * maybe go to a class, don't want these to be copied by value. */
        class Shelf
        {
            // today's music ain't got the same shelf
            //public int index; // starting from top - needed?
            public int height;
            public int freeSpace;
            public List<BookPlace> contents;
            public CN lastCN;
            // would be nice to have aliases to first and last call number...

            /** Constructor updates parent bookshelf's available space. */
            public Shelf(int height, int length)
            {
                //this.index = 0; 
                this.height = height;
                this.freeSpace = length;
                this.contents = new List<BookPlace>();
                // shouldn't matter, won't check if shelf is empty
                this.lastCN = default(CN);
            }
        }

        Shelf[] shelves;

        /** Create a bookshelf with specified dimensions and number of shelves. */
        public Bookshelf(int numShelves, int shelfLength, int shelfDepth, int shelfHeight)
        {
            this.shelfLength = shelfLength;
            this.shelfDepth = shelfDepth;
            totalSpace = shelfLength * numShelves;
            freeSpace = totalSpace;
            shelves = new Shelf[numShelves]; // yes, you double-init arrays of objects.
            for (int i=0; i < numShelves; i++)
            {
                shelves[i] = new Shelf(shelfHeight, shelfLength);
            }
        }

        /** Insert a book at the correct position on the correct shelf */
        void shelve(Book book) {
            CN bookCN = (CN) book.getCN<CN>();
            for (int i=0; i < this.numShelves; i++) {
                // for now, fill from top down, so book can go on first empty shelf.
                if (shelves[i].contents.Count == 0)
                {
                    shelves[i].contents.Add(new BookPlace(book, 0));
                    /* A shelf should do this to itself. */
                    shelves[i].freeSpace -= book.width;
                    shelves[i].lastCN = bookCN;
                    // maybe don't need freeSpace for whole shelf.
                    this.freeSpace -= book.width;
                    return;
                }
                //else if ((CallNumber<CN>) book.getCN<CN>() <= 
                //           (CallNumber<CN>) shelves[i].lastCN) // MYSTERIOUS
                int comp = (bookCN.CompareTo(shelves[i].lastCN));
                if (comp <= 0)
                {
                    int pos = shelves[i].contents.Count - 1;
                    //int pos = shelves[i].contents.Count / 2;
                    // binary search! No, linear for now. It's within a shelf, anyway.
                    while (shelves[i].contents[pos].theBook.getCN<CN>() != null 
                           && pos > 0)
                    {
                        pos--;
                    }
                    shelves[i].contents.Insert(pos, new BookPlace(book, pos));
                }

            }
            // should return a book place?
            // rule: if goes 'between' shelves, put in the one that has most space.
            // need a special case for overflow or do it automatically?
        }

        /** Presort a list of books and then divide evenly among shelves. */
        void shelveAll(List<Book> books)
        {
            // return list of books not shelves (wrong size, no classification, overflow)
            // overflow should go to next shelf!
        }

        /** rearrange books on shelf. */
        void redistribute() { }

        /** Restore a book that's already been shelved and pulled */
        void reshelve(Book book) {
            // return false if already there or not on shelf?
        }

        /** Get a reference to a book at a certain location */
        Book bookAt(int x, int y) {
            // actually want to return a place marker?
            // maybe change bookplace to have a fetchBook() method
            return null;
        }
        // TODO: fix to make it generic
        BookPlace find(CN cn) {
            return null;
            // should I use a hash table? maybe not for this but for a book?
        }

        /** Remove book at a certain place. mark as not onShelf */
        Book pull(int shelfNum, int offset)
        {
            // TODO: handle exception
            BookPlace place = shelves[shelfNum].contents.ElementAt(offset);
            // Will it ever be null?
            if (place != null && place.onShelf) {
                place.onShelf = false;
                return place.theBook;
            }
            return null;
        }
    }
}
