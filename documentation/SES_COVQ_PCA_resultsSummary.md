# Brief summary of PCA results for SES and Covid questionnaires


### SES
- There are 11 SES questions asked in both phases.
- After looking at correlograms, we removed 1) current bill help because it was very unrelated to the others which could be the result of differences in the way participants interpreted the question and 2) current responsibilities because it categorical or ordered. 
- We found that Phase 1 and phase 2 SES showed broadly similar patterns.
- We are using PC1 from the PCA which accounts for roughly 35% of the variance (results shown below) and these results are similar across phase 1 and phase 2. PC1 gives different weight to different SES questions.
#### SES PCA Phase 1:

<img width="787" alt="Screen Shot 2021-12-07 at 12 41 04 PM" src="https://user-images.githubusercontent.com/19710394/145095265-54fae1b1-2151-425a-aee8-d1b05c17ac5b.png">
<img width="1289" alt="Screen Shot 2021-12-07 at 12 40 11 PM" src="https://user-images.githubusercontent.com/19710394/145095094-0545b1a6-eec2-485d-bf87-70421b610a69.png">

#### SES PCA Phase 2:
<img width="816" alt="Screen Shot 2021-12-07 at 12 42 17 PM" src="https://user-images.githubusercontent.com/19710394/145095374-ec757a69-b0c3-40d2-be91-a602cdc82a3e.png">
<img width="1263" alt="Screen Shot 2021-12-07 at 12 42 30 PM" src="https://user-images.githubusercontent.com/19710394/145095378-70e1ec54-abed-4f61-a0ad-dc280d8534ad.png">



### COVID Qs
- There were 9 covid questions asked in both phases
- After looking at correlograms, the top five covid qs that stand out are personal risk, threat, personal death, death of other and know any one who tested positive. However, the know anyone who tested positive is coded funky and its unclear whats happening there. The change across phases for the top four (throwing out know anyone who tested positive right now) is pretty small, but the changes in personal risk distribution are related to RDM and the distribution of changes across phases is widest (ie it has the biggest change) and may be the variable to go with. The top four variables are highly related and the degree to which they change across phases is also correlated. 
- highest variance is other person die followed by personal risk ( similar)
- Change in personal risk and death of other are not as correlated as we'd think given their relationship within each phase. This could be that the change is just noise and so there is no meaningful shift that is happening. 
- PCA used 4 out of the 9 covid questions: personal risk, threat, personal death, and death of other (explained above)
- We are going ahead using personal risk variable for now because component one weighs each of the four components roughly equally and accounts for 60% of the variance. This could mean that picking one variable (i.e. personal risk) would be ok and the argument for would be that its more straightforward and more interpretable. The counter argument would be to use the weighted average of the four variables.
- pattern is similar across phases

#### COVID Q PCA Phase 1:
<img width="768" alt="Screen Shot 2021-12-07 at 12 48 52 PM" src="https://user-images.githubusercontent.com/19710394/145096261-baee2387-be91-4f52-a9d2-2fba0ddd637f.png">

#### COVID Q PCA Phase 2:
<img width="737" alt="Screen Shot 2021-12-07 at 12 49 04 PM" src="https://user-images.githubusercontent.com/19710394/145096269-06d5a712-af7c-42f5-8954-efbbc0b18706.png">
