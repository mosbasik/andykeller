#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>

//http://stackoverflow.com/questions/236129/how-to-split-a-string-in-c
#include <string>
#include <sstream>
#include <vector>

using namespace std;

//http://stackoverflow.com/questions/236129/how-to-split-a-string-in-c
//http://www.cplusplus.com/forum/general/13135/
vector<int> &split(const string &s, char delim, vector<int> &elems) {
    stringstream ss(s);
    string item;
    int int_item;
    while(getline(ss, item, delim)) {
        int_item = atoi(item.c_str());
        if(!item.empty()) {
            elems.push_back(int_item);
        }
    }
    return elems;
}

//http://stackoverflow.com/questions/236129/how-to-split-a-string-in-c
vector<int> split(const string &s, char delim) {
    vector<int> elems;
    split(s, delim, elems);
    return elems;
}

int main() {
    string myString = "This is a string with spaces in it.";
    string data = "2335 4 5831 3";
    vector<int> foo = split(data, ' ');

    //http://stackoverflow.com/questions/10750057/c-printing-out-the-contents-of-a-vector
    for(vector<int>::const_iterator i = foo.begin(); i != foo.end(); ++i) {
        cout << *i << " ";
    }
}