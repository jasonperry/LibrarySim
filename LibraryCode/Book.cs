using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LibraryCode
{
    /** A book. */
    public class Book  //<DeweyCallNumber>, IComparable<LOCCallNumber>
    {
        public int width { get; }
        public int depth { get; } // across the cover.
        public int height { get; }
        // can have multiple classifications, but comparisons need to be explicitly
        //  implemented
        List<CallNumber> callNumbers; 

        // TODO: properly represented MARC or BibFrame fields.
        // A nested struct for that, which is passed into the constructor.
        // And a pointer to the location of the book files
        public Book(int width, int depth, int height, CallNumber callNumber)
        {
            this.width = width;
            this.depth = depth;
            this.height = height;
            callNumbers.Add(callNumber);
        }

        /** Get call number of type determined by type parameter. */
        public CallNumber getCN<TCallNum>()
        {
            foreach(CallNumber cn in callNumbers) {
                if (cn.GetType() == typeof(TCallNum)) {
                    return cn;
                }
            }
            throw new CallNumberException("Book does not have Call Number of given type");
        }

        /** Get the Dewey Decimal call number. */
        public DeweyCallNumber getDDC()
        {
            foreach (CallNumber cn in callNumbers)
            {
                if (cn.GetType() == typeof(DeweyCallNumber)) {
                    return (DeweyCallNumber) cn;
                }
            }
            throw new CallNumberException("Book does not have Dewey Call Number");
        }

        /** Get the Library of Congress Call number */
        public LOCCallNumber getLOC()
        {
            foreach (CallNumber cn in callNumbers)
            {
                if (cn.GetType() == typeof(LOCCallNumber)) {
                    return (LOCCallNumber)cn;
                }
            }
            throw new CallNumberException("Book does not have LOC Call Number");
        }
        // Maybe not needed.
        bool hasCallNumber(Type classificationType) { return false; }

        // Hopefully can use this as comparer
        public int Compare<T>(Book other)
        {
            return this.getCN<T>().CompareTo(other.getCN<T>());
        }

        /* int IComparable<DeweyCallNumber>.CompareTo(DeweyCallNumber other)
        {
            throw new NotImplementedException();
        }

        int IComparable<LOCCallNumber>.CompareTo(LOCCallNumber other)
        {
            throw new NotImplementedException();
        } */

        public int CompareTo<T>(object obj)
        {
            throw new NotImplementedException();
        }
    }
}
