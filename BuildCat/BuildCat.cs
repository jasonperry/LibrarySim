using System;
using System.IO;
using System.Xml.Serialization;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LibraryCode;

namespace BuildCat
{
    /** Command-line utility to create a book catalog from data files
     *  (starting with MarcXML). */
    class Program
    {
        static recordType ProcessMarcXMLFile(string filePath)
        {
            Console.WriteLine("Processing xml file " + filePath);
            XmlSerializer ser = new XmlSerializer(typeof(recordType));
            recordType record = 
                ser.Deserialize(new FileStream(filePath, FileMode.Open)) 
                    as recordType;

            // make a book of it? maybe not here.
            if (record == null) {
                Console.WriteLine("Error processing MarcXML file " + filePath);
            }
            return record;
        }

        /** Just to get familiar with the fields. */
        static void PrintMarcXML(recordType record)
        {
            Console.WriteLine(record.leader.id);
            foreach (dataFieldType field in record.datafield) {
                if (field.tag == "245") {
                    Console.WriteLine("Found title field!");
                    continue;
                }
                if (field.tag == "300") {
                    Console.WriteLine("Found dimensions field!");
                }
            }
        }

        static Book RecordToBook(recordType record)
        {
            // author and title
            string authorSort = "";
            string title = "";   // TODO: think about how to handle missing data.
            // TODO: attempt to set from record.
            int widthmm = 20, depthmm = 180, heightmm = 230;

            foreach(dataFieldType field in record.datafield) {
                if (field.tag == "100") {
                    foreach (subfieldatafieldType subField in field.subfield) {
                        if (subField.code == "a") {
                            Console.WriteLine("Found author field.");
                            authorSort = subField.Value;
                        }
                    }
                }
                if (field.tag == "245") {
                    foreach (subfieldatafieldType subField in field.subfield) {
                        if (subField.code == "a") {
                            Console.WriteLine("Found title field.");
                            title = subField.Value;
                        }
                    }
                }
            }
            // create an alpha call number from the author and title.
            var callNum = new AlphaStringsCallNumber(authorSort + " " + title);

            // might throw 'insufficient data' exception.

            var book = new Book(widthmm, depthmm, heightmm, callNum);
            // here goes code to add new call numbers.
            return book;

        }

        static void Main(string[] args)
        {
            if (args.Length != 2) {
                // COMMAND-LINE ARGUMENTS: folder of MarcXMLs, CN type
                Console.WriteLine("catalog builder usage.");
                Console.WriteLine(
                    "buildcat.exe <MarcXML folder> <--loc|--dewey|--alpha>");
                return; // exit program?
            }

            var scanPath = args[0];
            var cnType = args[1];
            Console.WriteLine(scanPath + ", " + cnType);
            // NOTE: can't follow windows .lnk files
            if (Directory.Exists(scanPath)) {
                Console.WriteLine("yah!");
                string[] fileEntries = Directory.GetFiles(scanPath);
                int filesFound = 0;
                int filesSuccess = 0;
                foreach (string fileName in fileEntries) { 
                    if (fileName.EndsWith(".xml")) {
                        filesFound++;
                        var record = ProcessMarcXMLFile(fileName);
                        if (record != null) {
                            filesSuccess++;
                            PrintMarcXML(record);
                        }
                        else {
                            Console.WriteLine("Processing file {0} failed.",
                                              fileName);
                        }
                    }
                }
                Console.WriteLine("Processed {0} files, {1} successful",
                                  filesFound, filesSuccess);
            }
            else {
                Console.WriteLine("Error: not a directory");
                return;
            }

        }
    }
}
