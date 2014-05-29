function [ ] = read_and_reverse_mm ( infile, outfile, comment,field )
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here

mat = mmread(infile);
mmwrite(outfile, mat(:, [2, 1, 3]), comment, field );

end

