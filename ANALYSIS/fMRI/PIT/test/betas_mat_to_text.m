% Exctract betas to .txt FOR PIT 

% create text file with colons: ID, & Betas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dbstop if error

%% def path
cd ~
home = pwd;
homedir = [home '/REWOD'];

%% def var
task = 'PIT'; %
threshold = '0.01';
con_name = 'CSp_CSm';

%% ROI NAMES
ROI1 = 'CSp-CSm_Nacc_Left_betas.mat';
ROI2 = 'CSp-CSm_Nacc_Right_betas.mat';
ROI3 = 'CSp-CSm_vmPFC_Left_betas.mat';
ROI4 = 'CSp-CSm_vmPFC_Right_betas.mat';


%% create database

in_dir        = fullfile (homedir, '/DERIVATIVES/ANALYSIS/', task, 'ROI', threshold, con_name);
out_dir   = fullfile(homedir, '/DERIVATIVES/ANALYSIS/', task, 'ROI');


ID     = {'01';'02';'03';'04';'05';'06';'07';'09';'10';'11';'12';'13';'14';'15';'16';'17';'18';'20';'21';'22';'23';'24';'25';'26'};

cd (in_dir)

load(ROI1);
result(1,:) = [];
Nacc_Left = result;

load(ROI2);
result(1,:) = [];
Nacc_Right = result;

load(ROI3);
result(1,:) = [];
vmPFC_Left = result;

load(ROI4);
result(1,:) = [];
vmPFC_Right = result;


database = table(ID, Nacc_Left, Nacc_Right, vmPFC_Left, vmPFC_Right);

cd (out_dir)

writetable(database,'extracted_betas_CSp_CSm.txt','Delimiter','\t');
