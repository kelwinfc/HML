#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/highgui/highgui.hpp"
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <vector>

using namespace cv;
using namespace std;

//g++ -o convert convert.cpp `pkg-config opencv --cflags --libs`

int main(int argc, char* argv[]){
    argc--;
    argv++;
    
    Mat img = imread(argv[0]);
    
    {
        Mat aux;
        resize(img, aux, Size(256,256));
        aux.copyTo(img);
    }
    
    imshow("img", img);
    waitKey();
    
    ofstream fout(argv[1]);
    fout << img.rows << " " << img.cols << endl;
    
    vector<Mat> channels;
    split(img,channels);
    
    for (int r=0; r<img.rows; r++){
        for (int c=0; c<img.cols; c++){
            for (int ch=0; ch<3; ch++){
                fout << (int)channels[ch].at<uchar>(r,c) << " ";
            }
            fout << endl;
        }
    }
}
