#include "burstSort.h"

using namespace std;

int main(int argc, char * argv[]){
	if(argc < 4 || argc > 5){
		cerr << "Usage: sort numOfElements inputFile outputFile\n"
		<< "options: `-serial` for serial execution\n";
		exit(EXIT_FAILURE);
	}

	int count = atoi(argv[1]);
	char*  inputFileName = argv[2];
	char* outputFileName = argv[3];
	bool serial = (argc == 5 && 0 == strcmp("-serial",argv[4]));

	cout << "INPUTFILE: " << inputFileName << endl
	     << "OUTPUTFILE: " << outputFileName << endl
	     << "NUMBER OF ENTRIES: " << count << endl;

	ifstream inputFile;
	ofstream outputFile;

	inputFile.open(inputFileName, ios::in | ios::binary);
	if (!inputFile) {
		cerr << "Can't open input file " << inputFileName << endl;
		exit(1);
	}

	outputFile.open(outputFileName, ios::out | ios::binary);

	if (!outputFile) {
		cerr << "Can't open output file " << outputFileName << endl;
		exit(1);
	}

	BurstSort bs(count);
	
	char line[ENTRY_LENGTH + 1];
	line[ENTRY_LENGTH] = '\0';
	
	for(int i = 0; i < count; i++){
		inputFile.read(line,ENTRY_LENGTH);
		bs.insert(line);
	}

	bs.sortAndPrint(serial,outputFile);

	inputFile.close();
	outputFile.close();
}