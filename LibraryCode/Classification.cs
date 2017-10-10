using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LibraryCode
{

    public class CallNumberException : Exception
    {
        public CallNumberException(string message) : base(message)
        { }
    }

    public abstract class CallNumber// <Derived> //: IClassification
    {
        public CallNumber(string cnStr) { this.cnStr = cnStr; }
        // remember, 'protected' lets derived classes access it.
        public string cnStr { get; }  

        // "What you can't do is force a derived class to implement a specific constructor signature."
        // http://stackoverflow.com/questions/2299037/abstract-constructor-in-c-sharp

        public abstract List<string> subject();

        // public abstract int CompareTo(CallNumber<Derived> other);
        public abstract int CompareTo(CallNumber obj);

        // weird, still don't fully understand the template stuff.
        // this compiles, but I can't use it to compare two call numbers...
        // Operators only in the subclass
        public static bool operator <= (CallNumber c1, CallNumber c2)
        {
            // This will call the subclass method when overridden.
            return c1.CompareTo(c2) <= 0;
        }
        public static bool operator >=(CallNumber c1, CallNumber c2)
        {
            return c1.CompareTo(c2) >= 0;
        }

    }  

    // TODO: static dewey classification object here? when to initialize?

    // I don't have to specify the interface on the base class?
    public class DeweyCallNumber : CallNumber// <DeweyCallNumber> //, IClassification
                                   
    {
        // TODO: only return a string (in base), make rest of fields private.
        public decimal cn { get; }
        public string authorLastname { get; }
        public string cutterNumber;
        public int year { get; }
        private static string[] topLevelSubjects; 

        // have a static constructor to load the data.
        static DeweyCallNumber()
        {
            topLevelSubjects = new string[] {
                "Computer science, information & general works",
                "Philosophy & psychology",
                "Religion",
                "Social sciences",
                "Language",
                "Science",
                "Technology",
                "Arts & recreation",
                "Literature",
                "History & geography" };
        }
        public DeweyCallNumber(String cnStr) : base(cnStr)
        {
            string[] split = cnStr.Split(new char[]{' '});
            decimal d;
            // TODO: check for space with year/author following
            if (decimal.TryParse(cnStr, out d))
            {
                cn = (decimal)d;
            }
            else goto fail;
            fail:
            {
                throw new CallNumberException
                    ("Could not parse string " + cnStr + " as Dewey Decimal");
            }
            
        }

        public override List<String> subject()
        {
            return new List<String> { topLevelSubjects[((int)this.cn) / 100] };
        }

        // Two CompareTo's? One for interface and one for abstract class?
        /*public int CompareTo(DeweyCallNumber other)
        {
            // decimal comparison.
            // TODO: throw exception if not right type. Or let it break?
            return cn.CompareTo(other.cn);
        } */

        public override int CompareTo(CallNumber other)
        {
            int comp = cn.CompareTo(((DeweyCallNumber)other).cn);
            // check and throw if other isn't Dewey? Or will this do it?
            if (comp != 0) return comp;
            return authorLastname.CompareTo(((DeweyCallNumber)other).authorLastname);
        }

    }
    
    public class LOCCallNumber : CallNumber //<LOCCallNumber>
    {
        private string letters;
        private string num;
        public LOCCallNumber(String cnStr) : base(cnStr)
        {
        }
        public override int CompareTo(CallNumber obj)
        {
            throw new NotImplementedException();
        }

        public override List<string> subject()
        {
            throw new NotImplementedException();
        }
    } 

    /** List of strings, such as category, author, or lastname, first */
    public class AlphaStringsCallNumber : CallNumber 
    {
        private List<string> cnStrings;

        public AlphaStringsCallNumber(String cnStr): base(cnStr) {
            // TODO: split by whitespace and store in the list
            var wspace = new char[] {' ', '\t', '\n'};
            var sarray = (cnStr.ToLower().Split(wspace, StringSplitOptions.RemoveEmptyEntries));
            if (sarray.Length == 0) {
                throw new CallNumberException("Empty alphabetic call number");
            }
            cnStrings = new List<string> (sarray); 
        }

        public override int CompareTo(CallNumber other) {
            var alphOther = (AlphaStringsCallNumber) other;
            for (int i=0; i < this.cnStrings.Count; i++) {
                if (alphOther.cnStrings.Count == i) return 1; // this is longer
                int comp = this.cnStrings[i].CompareTo(alphOther.cnStrings[i]);
                if (comp != 0) return comp;
            }
            // Only possibility is other is longer (thus greater) or they're equal.
            if (alphOther.cnStrings.Count > this.cnStrings.Count) return -1;
            return 0; // equal
        }

        public override List<string> subject()
        {
            throw new NotImplementedException();
        }
    }

}

/***********************************************
  failed tries
  ********************************************** */
    // new try
    /* public abstract class CallNumber
    {
        public static bool operator <=(CallNumber n1, CallNumber n2)
        {
            return leq(n1, n2);
        }
        public static bool operator >=(CallNumber n1, CallNumber n2)
        {
            return !leq(n1, n2);
        }
        // can't be abstract. Oh well.
        public static bool leq(CallNumber n1, CallNumber n2)
        {
            return false;
        }
    } */
    /*    public interface IClassification : IComparable
    //public interface IClassification<TCallNum>: IComparable<TCallNum> // interface inheritance! Ha!
    {
        List<string> subject();
        // Can't declare operators in interfaces :(
        //public static bool operator <= (TCallNum c1, TCallNum c2);

        // if explicitly implemented it's private! Can only call after
        //   casting to an interface object.

        // no visibility modifier in interfaces?
        //int CompareTo(TCallNum other);
    } */

    // MAYBE GIVE UP AND JUST HAVE A SINGLE CLASS WITH A 'CN TYPE' ENUM FIELD,
    //  AND ONLY COMPARE IF THEY MATCH.
    // ...but before that, try something with just interfaces?
    // Yes! interfaces can also have type parameters! Think of how that might help!
    //   can I pass an interface the class name itself?
    //  then do Bookshelf<T> where T : IClassification<T>
    //   or Bookshelf<T,U> where T: IClassification<U>  nah.

    // can to generic methods with type constraints, and 'naked' constraints

    // self-referenceing generic declarations:
    // public interface IClassification<T> ...
    // public class DeweyDecimalCN : IClassification<DeweyDecimalCN>

    // Curiously Recurring Template Pattern.
    // http://stackoverflow.com/questions/2032636/how-to-make-an-abstract-base-class-icomparable-that-doesnt-compare-two-separate

    // any 'where' constraints needed