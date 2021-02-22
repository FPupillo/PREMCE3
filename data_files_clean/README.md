


## Code book - Description of the variables 

## *ses-01_part-01 and ses-01_part-01*

### participant
participant number

### pract_resp.keys
key pressed by participant as a response in the practice trials

### pract_resp.corr
binary variable indicating whether the response was correct (1) or incorrect (0). In the practice trials. 

### first_butterfly
The first butterfly that appeared on each trial

### second_butterfly
The second butterfly that appeared on each trial

### Cue
Number appearing after the presentation of the two butterflies on every trial telling participants for which butterfly we want them to make the prediction. 

### left_flower
For each trial, the flower that appeared on the first position on the left

### top_flower
For each trial, the flower that appered on the second position on the left

### bottom_flower
For each trial, the flower that appeared on the third position starting from the left side of the screen. 

### right flower
For each trial, the flower that apperead on the fourth position starting from the left side of the screen. 

### corr_ans
The correct response for each trial. 

### obj_file
Image showed at the center of the screen

### trial_cond
Binary variable indicating whether it is a trial that occurs 85% of the time (1) or 15% of the time (0)

### task_resp.keys
Keys pressed by participants in the actual trials

### task_resp.corr
Variable indicating whether a reponse in the task was correct (1) or incorrect (0)

### task_resp.rt
Reaction time for every response

### switch_cond
Numeric variable indicating the block. The levels are the following:
99: warm-up block in ses-02_part-01
2: list A-plus (list A repeated on Day2)
3: list B. List that occurs after the first switch
4: List A two. switch back to list A again 
5: ListB two. Switch back to list B. There is a mistake for this list: it appears as 3 again. But it is recognizable as it is the second time the list 3 appears. 

### task_resp_2.keys
Response key for the question at the end of the first block (which flower was associated with which butterfly)

### task_resp_2.corr
Correct or incorrect response the question

### task_resp_2.rt
Reaction time for the last question

## *ses-01_part-01 and ses-02_part-01*

### participant
participant number

### warmup_resp.keys
key pressed by participant as a response in the warmup trials

### warmup_resp.corr
binary variable indicating whether the response was correct (1) or incorrect (0). In the warmpu trials. 

### warmup_resp.rt
Rt for the warmup trials

### first_butterfly
The first butterfly that appeared on each trial

### second_butterfly
The second butterfly that appeared on each trial

### Cue
Number appearing after the presentation of the two butterflies on every trial telling participants for which butterfly we want them to make the prediction. 

### left_flower
For each trial, the flower that appeared on the first position on the left

### top_flower
For each trial, the flower that appered on the second position on the left

### bottom_flower
For each trial, the flower that appeared on the third position starting from the left side of the screen. 

### right flower
For each trial, the flower that apperead on the fourth position starting from the left side of the screen. 

### corr_ans
The correct response for each trial. 

### obj_file
Image showed at the center of the screen

### trial_cond
Binary variable indicating whether it is a trial that occurs 85% of the time (1) or 15% of the time (0)

### task_resp_3.keys
Keys pressed by participants in the task blocks

### task_resp_3.corr
Variable indicating whether a reponse in the task was correct (1) or incorrect (0)

### task_resp_3.rt
Reaction time for every response

### switch_cond
Numeric variable indicating the block. The levels are the following:
99: warm-up block in ses-02_part-01
2: list A-plus (list A repeated on Day2)
3: list B. List that occurs after the first switch
4: List A two. switch back to list A again 
5: ListB two. Switch back to list B. There is a mistake for this list: it appears as 3 again. But it is recognizable as it is the second time the list 3 appears. 


## *ses-02_part-02*

### participant
Participants' number

### recog_resp.keys
Response (left= old item, right = new item)

### recog_resp.rt
Reaction time for recognition

### conf_resp.keys
confidence ratings. Levels are the following: 
1 = Very unsure
2 = Unsure
3 = Sure
4 = Very Sure

### conf_resp.rt
Reaction time for confidence

### images
Image displayed on every trial

### corr_ans
Correct answer for every trial

### type
Variable indicating whether the image displayed is old or new




