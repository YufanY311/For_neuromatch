%% Function for reading behavioral log xlsx file
function [newstruc] = readexcel(x)
filename = x;
[num, txt, raw] = xlsread(filename);
fieldNames = raw(1,:);
data = raw(2:end,:); 
newstruc = struct;
for i = 1:size(fieldNames,2)
    newstruc.(fieldNames{i}) = data(:,i);
end
end