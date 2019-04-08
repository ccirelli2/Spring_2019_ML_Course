clear all;
close all;
I = double(imread('coins.png'));

figure('position',[100 100 1300 600])
subplot(151);
imagesc(I);colormap gray;

K = [ -1 1];
IK = conv2(I,K,'same');
subplot(152);
imagesc(IK);colormap gray;

K = [ -1; 1];
IK = conv2(I,K,'same');
subplot(153);
imagesc(IK);colormap gray; 


K = [ 0 -1 0;
      -1 4 -1;
      0 -1 0 ];
IK = conv2(I,K,'same');
subplot(154);
imagesc(IK);colormap gray;


K = ones(5,5);
IK = conv2(I,K,'same');
subplot(155);
imagesc(IK);colormap gray;