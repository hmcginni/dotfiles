function mdirs
%
% MDIRS - list all directories in the directory stack
%

%% Get Directory Stack

dir_stack = getenv('DIR_STACK');
dir_split = strsplit(dir_stack, ':');
dir_list = string([dir_split,pwd]);

%% Display Directory Stack

disp(dir_list);