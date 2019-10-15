% Exctract betas to .txt FOR HED 

% create text file with colons: ID, & Betas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dbstop if error

%% def path
cd ~
home = pwd;
homedir = [home '/REWOD'];

%% def var
task = 'hedonic'; %
threshold = '0.01';
%con_name = 'R_C';
con_name = 'R_N';

%% ROI NAMES
% ROI1 = 'reward-control_AMY_Left_betas';
% ROI2 = 'reward-control_AMY_Right_betas';
% ROI3 = 'reward-control_Piri_Left_betas.mat';
% ROI4 = 'reward-control_Piri_Right_betas.mat';
ROI1 = 'reward-neutral_Nacc_Left_betas';
ROI2 = 'reward-neutral_Nacc_Right_betas';
ROI3 = 'reward-neutral_subcal_Left_betas.mat';
ROI4 = 'reward-neutral_subcal_Right_betas.mat';


%% create database

in_dir        = fullfile (homedir, '/DERIVATIVES/ANALYSIS/', task, 'ROI', threshold, con_name);
out_dir   = fullfile(homedir, '/DERIVATIVES/ANALYSIS/', task, 'ROI');


ID     = {'01';'02';'03';'04';'05';'06';'07';'09';'10';'11';'12';'13';'14';'15';'16';'17';'18';'20';'21';'22';'23';'24';'25';'26'};

cd (in_dir)

% load(ROI1);
% result(1,:) = [];
% AMY_Left = result;
% 
% load(ROI2);
% result(1,:) = [];
% AMY_Right = result;

load(ROI1);
result(1,:) = [];
Nacc_Left = result;

load(ROI2);
result(1,:) = [];
Nacc_Right = result;

load(ROI3);
result(1,:) = [];
subcal_Left = result;

load(ROI4);
result(1,:) = [];
subcal_Right = result;


database = table(ID, Nacc_Left, Nacc_Right, subcal_Left, subcal_Right);

cd (out_dir)

writetable(database,'extracted_betas_R_N.txt','Delimiter','\t');
