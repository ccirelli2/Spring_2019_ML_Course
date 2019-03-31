close all;
clear all;

load splineData;

q = 1; %linear spline
xi = [-2, 1]; % the location of the cutting points
% constructing the feature matrix
X = [ones(1000,1), x, x.^2, x.^3, max(x-xi(1),0).^q, max(x-xi(2),0).^q];
beta_lin = inv(X'*X)*X'*y;

% plotting the fitted curve
xg = linspace(-4,4,1000)';
Xg = [ones(1000,1), xg, xg.^2, xg.^3, max(xg-xi(1),0).^q, max(xg-xi(2),0).^q];
yg = Xg*beta_lin;

subplot(121);
plot(x,y,'*');
hold on;
plot(xg,yg,'r','linewidth',4);title('spline: q = 1')

q = 3; %linear spline
xi = [-2, 1]; % the location of the cutting points
% constructing the feature matrix
X = [ones(1000,1), x, x.^2, x.^3, max(x-xi(1),0).^q, max(x-xi(2),0).^q];
beta_cub = inv(X'*X)*X'*y;

% plotting the fitted curve
xg = linspace(-4,4,1000)';
Xg = [ones(1000,1), xg, xg.^2, xg.^3, max(xg-xi(1),0).^q, max(xg-xi(2),0).^q];
yg = Xg*beta_cub;

subplot(122);
plot(x,y,'*');
hold on;
plot(xg,yg,'g','linewidth',4);title('spline: q=3')

