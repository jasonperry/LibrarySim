using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LibraryCode
{
    /** A book. */
    public class Book
    {
        public int length { get; }
        public int width { get; }
        public int height { get; }
        // can have multiple classifications.
        List<IClassification> callNumbers; 
        // TODO: properly represented MARC or BibFrame fields.
        // A Record subtype for that, which is passed into the constructor.

        public Book(int length, int width, int height, IClassification callNumber)
        {
            this.length = length;
            this.width = width;
            this.height = height;
            callNumbers.Add(callNumber);
        }

        /** Get call number of type determined by type parameter. */
        public IClassification getCN<CN>()
        {
            foreach(IClassification cn in callNumbers)
            {
                if (cn.GetType() == typeof(CN))
                {
                    return cn;
                }
            }
            throw new CallNumberException("Book does not have Call Number of given type");
        }

        public DeweyCallNumber getDDC()
        {
            foreach (IClassification cn in callNumbers)
            {
                if (cn.GetType() == typeof(DeweyCallNumber))
                {
                    return (DeweyCallNumber) cn;
                }
            }
            throw new CallNumberException("Book does not have Dewey Call Number");
        }
        bool hasCallNumber(Type classificationType) { return false; }
    }
}
