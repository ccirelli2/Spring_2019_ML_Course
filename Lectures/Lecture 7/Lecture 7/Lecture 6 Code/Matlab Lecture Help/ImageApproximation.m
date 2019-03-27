clear all;
close all;
X = double(rgb2gray(imread('street.jpg')));

[U, Sig, V] = svd(X);

figure('position',[100 200 1500 700]);
subplot(121);
imagesc(X);colormap gray

Z = zeros(size(X));

for i = 1:328
    subplot(122);
    imagesc(Z);title(strcat('r=',num2str(i-1)));
    pause;
    Z = Z + Sig(i,i)*U(:,i)*V(:,i)';
end
