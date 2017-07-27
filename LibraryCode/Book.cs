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
        public int length { get; }
        public int width { get; }
        public int height { get; }
        // can have multiple classifications, but comparisons need to be explicitly
        //  implemented
        List<CallNumber> callNumbers; 
        // TODO: properly represented MARC or BibFrame fields.
        // A Record subtype for that, which is passed into the constructor.

        public Book(int length, int width, int height, CallNumber callNumber)
        {
            this.length = length;
            this.width = width;
            this.height = height;
            callNumbers.Add(callNumber);
        }

        /** Get call number of type determined by type parameter. */
        public CallNumber getCN<TCallNum>()
        {
            foreach(CallNumber cn in callNumbers)
            {
                if (cn.GetType() == typeof(TCallNum))
                {
                    return cn;
                }
            }
            throw new CallNumberException("Book does not have Call Number of given type");
        }

        public DeweyCallNumber getDDC()
        {
            foreach (CallNumber cn in callNumbers)
            {
                if (cn.GetType() == typeof(DeweyCallNumber))
                {
                    return (DeweyCallNumber) cn;
                }
            }
            throw new CallNumberException("Book does not have Dewey Call Number");
        }

        public LOCCallNumber getLOC()
        {
            foreach (CallNumber cn in callNumbers)
            {
                if (cn.GetType() == typeof(LOCCallNumber))
                {
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
