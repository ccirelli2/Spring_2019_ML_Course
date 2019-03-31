clear all;
close all;
[b1, b2, b3] = meshgrid(linspace(-2,7,200),linspace(-2,3,200),linspace(-2,3,200));
b1 = b1(:);
b2 = b2(:);
b3 = b3(:);

load Data.mat;

q_terms = repmat(y,1,8e6) - X*[b1,b2,b3]';
RSS = sum(q_terms.^2);

L1Obj = ones(8e6,1);
L1ind = find(abs(b1) + abs(b2) + abs(b3)<1.5);
L1Obj(L1ind) = 0;

L2Obj = ones(8e6,1);
L2ind = find(b1.^2 + b2.^2 + b3.^2<1.7);
L2Obj(L2ind) = 0;


b1 = reshape(b1,[200 200 200]);
b2 = reshape(b2,[200 200 200]);
b3 = reshape(b3,[200 200 200]);
RSS = reshape(RSS,[200 200 200]);
L1Obj = reshape(L1Obj,[200 200 200]);
L2Obj = reshape(L2Obj,[200 200 200]);

figure('position',[100 100 1000 400])
subplot(121)
p = patch(isosurface(b1,b2,b3,RSS,300),'FaceAlpha',0.3);
isonormals(b1,b2,b3,RSS,p)
p.FaceColor = 'red';
p.EdgeColor = 'none';
daspect([1 1 1])
view(3); 



p = patch(isosurface(b1,b2,b3,RSS,500),'FaceAlpha',0.3);
isonormals(b1,b2,b3,RSS,p)
p.FaceColor = 'red';
p.EdgeColor = 'none';
daspect([1 1 1])
view(3); 


hold on;

p = patch(isosurface(b1,b2,b3,RSS,720),'FaceAlpha',0.3);
isonormals(b1,b2,b3,RSS,p)
p.FaceColor = 'red';
p.EdgeColor = 'none';
daspect([1 1 1])
view(3); 


p = patch(isosurface(b1,b2,b3,L1Obj,.5));
isonormals(b1,b2,b3,L1Obj,p)
p.FaceColor = 'green';
p.EdgeColor = 'none';
daspect([1 1 1])
view(3); 
axis([-2 7 -2 2 -2 3])
camlight; grid on
lighting gouraud
xlabel('\beta_1')
ylabel('\beta_2')
zlabel('\beta_3')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subplot(122)
p = patch(isosurface(b1,b2,b3,RSS,300),'FaceAlpha',0.3);
isonormals(b1,b2,b3,RSS,p)
p.FaceColor = 'red';
p.EdgeColor = 'none';
daspect([1 1 1])


p = patch(isosurface(b1,b2,b3,RSS,500),'FaceAlpha',0.3);
isonormals(b1,b2,b3,RSS,p)
p.FaceColor = 'red';
p.EdgeColor = 'none';
daspect([1 1 1])


hold on;

p = patch(isosurface(b1,b2,b3,RSS,720),'FaceAlpha',0.3);
isonormals(b1,b2,b3,RSS,p)
p.FaceColor = 'red';
p.EdgeColor = 'none';
daspect([1 1 1])


p = patch(isosurface(b1,b2,b3,L2Obj,.5));
isonormals(b1,b2,b3,L2Obj,p)
p.FaceColor = 'green';
p.EdgeColor = 'none';
daspect([1 1 1])
view(3); 
axis([-2 7 -2 2 -2 3])
camlight; grid on
lighting gouraud
xlabel('\beta_1')
ylabel('\beta_2')
zlabel('\beta_3')
