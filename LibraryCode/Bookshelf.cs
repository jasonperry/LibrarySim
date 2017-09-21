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

    /** A multi-shelf sorting bookcase with a call number type parameter. */
    public class Bookcase<TCallNum> : IComparer<Book> where TCallNum : CallNumber
    {
        /** Dimensions of the case itself. */
        public int width { get; protected set; }
        public int depth { get; protected set; }
        public int height { get; protected set; }

        public int numShelves { get; }
        /** Dimensions of shelves in this bookcase in mm. *
          * can have different height for every shelf. */
        int _shelfLength, _shelfDepth;
        /** Total linear mm of storage in bookcase. */
        int _totalSpace;
        /** Free space remaining in bookcase. */
        public int freeSpace { get; protected set; } = 0;  // no shelves at first
        /** First and last call numbers stored on this shelf. */
        TCallNum _startCallNumber, _endCallNumber;

        /** Nested class for the place on a shelf occupied by a book. */
        public class BookPlace
        {
            public Book theBook;
            public bool onShelf;
            public int pos; // millimeters from left.

            public BookPlace(Book book, int pos)
            {
                this.theBook = book;
                this.pos = pos;
                this.onShelf = false; 
            }
            public void replace() { onShelf = true; }
        }

        /** Nested class for a single linear shelf in a bookcase */
        class Shelf
        {
            // today's music ain't got the same shelf
            //public int index; // starting from top - needed?
            public int height;
            // length is from outer class
            public int freeSpace;
            public List<BookPlace> contents; // Maybe array for speed.
            public TCallNum lastCN;

            int lastpos = 0;
            // would be nice to have aliases to first and last call number...

            /** Constructor updates parent bookshelf's available space. */
            public Shelf(int height, int length) // if nested, shouldn't take length.
            {
                //this.index = 0; 
                this.height = height;
                this.freeSpace = length; 
                this.contents = new List<BookPlace>();
                // shouldn't matter, won't check if shelf is empty
                this.lastCN = default(TCallNum);
            }
            /** Add book to the end of the shelf. */
            public void Append(Book book)
            {
                if (freeSpace < book.width) {
                    throw new ShelvingException("Book of width " +
                        book.width.ToString() +
                        " does not fit on shelf with free space of " + 
                        freeSpace.ToString());
                }
                // create new bookplace
                var place = new BookPlace(book, lastpos);
                place.replace(); // put book there.
                // append bookplace to contents
                contents.Add(place);
                // update lastpos and freespace
                lastpos += book.width;
                freeSpace -= book.width;
            }
        }

        Shelf[] shelves;

        /** Create a bookshelf with specified dimensions and number of shelves. */
        public Bookcase(int numShelves, int shelfLength, int shelfDepth, int shelfHeight)
        {               // int sideMargin, bottomMargin, topMargin, shelfSeparation
            this.numShelves = numShelves;
            this._shelfLength = shelfLength;
            this._shelfDepth = shelfDepth;
            _totalSpace = shelfLength * numShelves;
            freeSpace = _totalSpace;
            shelves = new Shelf[numShelves]; // yes, you double-init arrays of objects.
            for (int i=0; i < numShelves; i++)
            {
                shelves[i] = new Shelf(shelfHeight, shelfLength);
            }
        }

        public bool empty() { return freeSpace == _totalSpace; }

        /** Insert a book at the correct position on the correct shelf 
          * TODO: Delayed. Shelving all at once is easier. */
        void shelve(Book book) {
            TCallNum bookCN = (TCallNum) book.getCN<TCallNum>();
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
                    while (shelves[i].contents[pos].theBook.getCN<TCallNum>() != null 
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

        // convenience method
        static int totalWidth(List<Book> books)
        {
            int total = 0;
            foreach (Book b in books)
            {
                total += b.width;
            }
            return total;
        }

        /** Presort a list of books and then divide evenly among shelves. */
        void shelveAllInitial(List<Book> books, out List<Book> overflow)
        {
            if (! this.empty()) {
                throw new ShelvingException
                    ("Can only call shelveAllInitial on empty Bookcase");
            }
            // Will throw error if any book doesn't have the right call number. 
            books.Sort(this);

            overflow = null;
            int startShelf = 0;
            // I thought to leave the top shelf empty.
            // if (totalWidth(books) <= _shelfLength) startShelf = 1;

            // Compute amount of each shelf to use. 
            // Try to leave some space and spread out nicely.
            int shelfLengthToUse = _shelfLength;
            float fullPercent = (float)totalWidth(books) / _totalSpace;
            if (fullPercent < 0.8)
            {
                shelfLengthToUse = (int)((fullPercent + 0.1) * _shelfLength);
            }

            int shelfi = startShelf;
            int curShelfUsed = 0;
            // loop thru BOOKS and update shelf index as necessary
            for (int i=0; i < books.Count; i++) 
            {
                curShelfUsed += books[i].width;
                if (curShelfUsed > shelfLengthToUse)
                {
                    shelfi++;
                    curShelfUsed = 0;
                    if (shelfi >= shelves.Count())
                    {
                        overflow = books.GetRange(i, books.Count() - i);
                        break; // yes, breaks out of the for loop.
                    }
                }
                shelves[shelfi].Append(books[i]);
            }
            // need to distinguish (wrong size, no classification, overflow)
            // Eventually: overflow on next (linked) shelf
        }

        /** rearrange books on case. */
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
        BookPlace find(TCallNum cn) {
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

        // Trick to make this class a comparer for the selected call number type.
        public int Compare(Book b1, Book b2)
        {
            return b1.Compare<TCallNum>(b2);
        }
    }
}
