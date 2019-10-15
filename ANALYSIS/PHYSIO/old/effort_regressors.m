% Create physiological regressors

% comment to my dataset: atttention participants 12 and 10 14 16 need to be
% processed by their own because the physiology stops slighlty before the
% end of the last EPI acquisition



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETTING THE PATH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

MRIdir = '/Users/admin/Documents/REWOD/REWOD/DATA/Brain/Population/subcor_norm/Apply_DispFields';
physiodir = '/Users/admin/Documents/REWOD/REWOD/DATA/Physiology';
run = {'R1'};


%subj={'S10'; 'S11';'S13'; 'S14'; 'S15'; 'S16'; 'S17'; 'S18'; 'S20'; 'S21'; 'S22'; 'S23'; 'S24'; 'S25'; 'S26'};
%physioID = {'s10'; 's11'; 's13'; 's14'; 's15'; 's16'; 's17'; 's18'; 's20'; 's21'; 's22'; 's23'; 's24'; 's25'; 's26'};

subj={'S1'};
physioID = {'s01'};


displayPlot = 1; % do we need a plot in the end ?

for  i=1:length(subj)
    subjX=subj(i,1);
    subjX=char(subjX); % subj{i,1}
    physioIDX = physioID (i,1);
    physioIDX = char(physioIDX);
    subjdir=fullfile(MRIdir, subjX); % subj{i,1}
    rundir = fullfile(subjdir, char(run)); % run{k}
    
    run1 = cell2mat(strfind (run, 'R1'));
    if run1
        physioRunID = '_PIT';
    else
        physioRunID = '_EVAL';
    end
    
    
    participantID = [physioIDX,physioRunID];
    
    fprintf('participant number: %s \n', subj{i})
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % OPEN FILE
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    cd (physiodir)
    
    filename = [num2str(participantID) '.acq'];
    physio = load_acq(filename);%load and transform acknoledge file
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % SET PARAMETERS ACCORDING TO THE SCANNER
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if run1
        
        % ENTER VARIABLES PARAMETERS for the PIT
        sampling_rate = 500; % number of measures per sec
        TR = 2.4;
        EPI = dir (fullfile(rundir, 'RL*.nii'));
        num_EPI = length(EPI);
        heart_channel = 1;
        resp_channel = 2;
        grip_channel = 3;
        MRI_volume = 4;
        num_channel = 4; %channels of interest
        
    else
        
        % ENTER VARIABLES PARAMETERS for the EVAL
        sampling_rate = 10000;
        TR = 2.4;
        cd (rundir) % count the EPI
        EPI = dir (fullfile(rundir, 'RL*.nii'));
        num_EPI = length(EPI);
        heart_channel = 3;
        resp_channel = 4;
        num_channel = 5; % channel of interest
        MRI_volume = 5;
        
    end
    
    cd (physiodir);
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % EXTRACT SIGNAL FOR TAPAS TOOLBOX AND SAVE INPUT VARIABLES
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %Sycronize the physiological data to the MRI session
    scanner_start = min (find((physio.data (:,MRI_volume)) == 5)); % First MRI volume trigger
    time_Physio = (length(physio.data (scanner_start:length(physio.data))))/sampling_rate; % The last image is acquired
    scanner_end = (num_EPI * TR) * sampling_rate;
    pyhsio_end = length(physio.data); %%% HERE INSERT THE LENGHT OF THE PHYSIO FILE: Physiotoolbox will calculate the exact length !!
    
    respEPI = physio.data (scanner_start:pyhsio_end,resp_channel); % resp belt wave form
    heartEPI = physio.data (scanner_start:pyhsio_end,heart_channel); % SpO wave form
    
    
    cd (rundir) % we save these variables in the subject directory with the nii images
    save (['respEPI_' subjX '.mat'], 'respEPI');
    save (['heartEPI_' subjX '.mat'], 'heartEPI');
    cd (physiodir)
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % CREATE AND SAVE THE EFFORT REGRESSOR
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if run1
        
        mxEPI = (TR*sampling_rate); %n of physio point measure per EPI
        scanner_end = max (find((physio.data (:,MRI_volume)) == 5)) + mxEPI;% we need to add + mxEPI because the trigger is at the begining of TR tno at the end
        %scanner_end = length(physio.data);
        gripEPI = physio.data (scanner_start:scanner_end,grip_channel); %exact length of the pyhsiological during the scanning session
        effort_regressor = nan (length(EPI),1);% initialize an empty vector
        cmpt = 1;
        
        for j= 1:length(EPI)
            %disp ([num2str(cmpt)]);
            if cmpt+mxEPI > length (gripEPI) % this should prevent matlab to crash if the physio is shorter than the scanning run
                x = length(gripEPI);
            else
                x = cmpt+mxEPI;
            end
            mean_effort = mean (gripEPI(cmpt:x));
            cmpt = cmpt+mxEPI;
            effort_regressor (j) = mean_effort;
        end
        
        
        cd (rundir)
        % save the regressor as a file text in the participant directory
        fid = fopen('regressor_effort.txt','wt');
        for ii = 1:length(effort_regressor)
            fprintf(fid,'%g\t',effort_regressor(ii));
            fprintf(fid,'\n');
        end
        fclose(fid);
        
    end
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % DISPLAY PLOT
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if displayPlot
        % Variable for Figure1
        Start = scanner_start;
        End = pyhsio_end;
        
        if run1
            % Figure run1
            figure
            for j = 1:num_channel
                subplot(num_channel,1,j);
                plot(physio.data (Start:End,j));
                switch j
                    case 1
                        title('SpO2 Wave form');
                    case 2
                        title('respiratory  Wave form');
                    case 3
                        title('hand grip');
                    case 4
                        title ('MRI_volume');
                end
            end
            
        else
            % figure run2
            figure
            for j = 1:num_channel
                subplot(num_channel,1,j);
                plot(physio.data (Start:End,j));
                switch j
                    case 1
                        title('raw corr');
                    case 2
                        title('raw zyg');
                    case 3
                        title('SpO2 wave form');
                    case 4
                        title ('respiratory wave form');
                    case 5
                        title ('MRI volume');
                end
            end
        end
    end 
end