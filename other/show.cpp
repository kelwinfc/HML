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
    
    ifstream fin(argv[0]);
    int n, m;
    
    fin >> n >> m;
    
    vector<Mat> channels;
    for (int ch=0; ch<3; ch++){
        Mat img(n,m,CV_8U);    
        channels.push_back(img);
    }
    
    for (int r=0; r<n; r++){
        for (int c=0; c<m; c++){
            for (int ch=0; ch<3; ch++){
                int value;
                fin >> value;
                channels[ch].at<uchar>(r,c) = (uchar)value;
            }
        }
    }
    
    Mat img;
    merge(channels, img);
    imshow("img", img);
    waitKey();
}