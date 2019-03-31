clear all
close all;
% loading some data with 3 features and 500 samples 
% (for now no response variable)
load DataX.mat;


figure('position',[100 200 1500 700]);
subplot(1,2,1)
scatter3(X(:,1),X(:,2),X(:,3),'MarkerFaceColor',[.8 0 0],...
    'MarkerEdgeColor','k');
xlabel('X1');
ylabel('X2');
zlabel('X3');

pause;
% we can see that all the points lie on a facet represented by
% X3 = 4/6*X1 -1/6*X2 
f1 = linspace(-10,10,100);
f2 = linspace(-15,15,100);
[F1, F2] = meshgrid(f1,f2);
hold on;
mesh(F1,F2,4/6*F1 -1/6*F2,'facecolor',[0 1 0],'facealpha',.4);

%plotting the principal coordinate vectors
quiver3([0;0],[0;0],[0;0],[-1;15],[-13;0],[3/2; 10],'linewidth',4,'color','k')
text(-1,-13,2,'Z1')
text(15,0,10,'Z2')

C = [-1 -13 1.5;15, 0 ,10]';
C(:,1) = C(:,1)/norm(C(:,1));
C(:,2) = C(:,2)/norm(C(:,2));

Z = X*C;

subplot(122);
scatter(Z(:,1),Z(:,2),'MarkerFaceColor',[.8 0 0],...
    'MarkerEdgeColor','k');
xlabel('Z1(X1,X2,X3)')
ylabel('Z2(X1,X2,X3)')