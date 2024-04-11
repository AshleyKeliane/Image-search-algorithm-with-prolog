% dataset(DirectoryName)
% this is where the image dataset is located
dataset('/Users/keliane/queryImages').   % replace with your queryImage directory!!!
% directory_textfiles(DirectoryName, ListOfTextfiles)
% produces the list of text files in a directory
directory_textfiles(D,Textfiles):- directory_files(D,Files), include(isTextFile, Files, Textfiles).
isTextFile(Filename):-string_concat(_,'.txt',Filename).
% read_hist_file(Filename,ListOfNumbers)
% reads a histogram file and produces a list of numbers (bin values)
read_hist_file(Filename,Numbers):- open(Filename,read,Stream),read_line_to_string(Stream,_),
                                   read_line_to_string(Stream,String), close(Stream),
								   atomic_list_concat(List, ' ', String),atoms_numbers(List,Numbers).
								   
% similarity_search(QueryFile,SimilarImageList)
% returns the list of images similar to the query image
% similar images are specified as (ImageName, SimilarityScore)
% predicat dataset/1 provides the location of the image set
similarity_search(QueryFile,SimilarList) :- dataset(D), directory_textfiles(D,TxtFiles),
                                            similarity_search(QueryFile,D,TxtFiles,SimilarList).
											
% similarity_search(QueryFile, DatasetDirectory, HistoFileList, SimilarImageList)
similarity_search(QueryFile,DatasetDirectory, DatasetFiles,Best):- read_hist_file(QueryFile,QueryHisto), 
                                            compare_histograms(QueryHisto, DatasetDirectory, DatasetFiles, Scores), 
                                            sort(2,@>,Scores,Sorted),take(Sorted,5,Best).

% compare_histograms(QueryHisto,DatasetDirectory,DatasetFiles,Scores)
% compares a query histogram with a list of histogram files 
compare_histograms(_, _, [], []).
compare_histograms(QueryHisto, DatasetDirectory, [DatasetFile | Rest], [(DatasetFile, Score) | RestScores]) :-
    atomic_list_concat([DatasetDirectory, DatasetFile], '/', FullPath), % Get full path to histogram file
    read_hist_file(FullPath, DatasetHisto),                           % Read histogram file
    histogram_intersection(QueryHisto, DatasetHisto, Score),          % Compute similarity score
    compare_histograms(QueryHisto, DatasetDirectory, Rest, RestScores).% Continue with remaining files
    

% histogram_intersection(Histogram1, Histogram2, Score)
% compute the intersection similarity score between two histograms
% Score is between 0.0 and 1.0 (1.0 for identical histograms)
histogram_intersection([], [], 1.0). % If both histograms are empty, they are identical (score = 1.0)
histogram_intersection(H1, H2, Score) :-
    % Calculate the sum of the minimum values for each bin in the histograms
    sum_min(H1, H2, SumMin),
    % Calculate the sum of all bin values in both histograms
    sum_list(H1, SumH1),
    sum_list(H2, SumH2),
    SumTotal is SumH1 + SumH2,
    % Calculate the similarity score
    (SumTotal =\= 0 -> Score is SumMin / SumTotal ; Score is 0). % Ensure SumTotal is not zero to avoid division by zero

% Calculate the sum of the minimum values for each bin in two histograms
sum_min([], [], 0).
sum_min([H1|T1], [H2|T2], SumMin) :-
    Min is min(H1, H2),
    sum_min(T1, T2, RestMin),
    SumMin is Min + RestMin.

% Calculate the sum of all bin values in a histogram
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, RestSum),
    Sum is H + RestSum.


% take(List,K,KList)
% extracts the K first items in a list
take(Src,N,L) :- findall(E, (nth1(I,Src,E), I =< N), L).
% atoms_numbers(ListOfAtoms,ListOfNumbers)
% converts a list of atoms into a list of numbers
atoms_numbers([],[]).
atoms_numbers([X|L],[Y|T]):- atom_number(X,Y), atoms_numbers(L,T).
atoms_numbers([X|L],T):- \+atom_number(X,_), atoms_numbers(L,T).