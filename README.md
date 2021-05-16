# MACE Classification

## Purpose
- 아래의 데이터를 활용하여 마취 방법별(TIVA, volatile)로 수술 후 MACE event가 발생하는지 분류  

## Data
- EMR(Electronic Medical Record)
- VR (Vital Record)
- AR (Anesthestic Record)

## Preprocessing
- EMR Preprocessing  
    1. 검사 결과에 대한 결측치는 검사를 하지 않아 기록이 되지 않은 것 확인 (pre-op evaluation)  
    2. PFT(폐기능 검사) 3개의 검사 수치를 파생변수로 생성 - 결측치는 0으로 대체  
    3. PFT, EF 검사 유무에 대한 파생 변수 생성  
    4. Method에 따라 잘못 입력된 Case ID 수정 
    5. 삭제한 변수 : RPR(매독검사), OP NAME, Intropics(승압제 투여여부), POD_1(Death event에 관한 것), PreopDx(수술 전 진단명)
    6. OP Time : 0 => 결측치인 것들은 VR 데이터로 부터 시간 계산하여 대체 
    7. BMI, Wt, Alb, BUN, Cr 등의 값이 자나치게 크거나 작은 경우 잘못 입력된 값이라고 간주하고 제거하거나 재입력
    8. volatile과 TIVA의 케이스가 잘못 분류된 경우 변경
    9. 수술명 별 중증도를 점수화하여 수술명 변경
 
 <br>
 
 - TIVA (VR)  
    1. EMR과 겹치는 Case 추출
    2. PROPOFOL, REMIFENTANIL, AGENT 변수만 사용
    3. AGENT 관련 결측치는 모두 0으로 처리 후, AGENT_ET 변수는 max 값이 1미만인 경우 모두 0으로 처리 
    4. PROPOFOL, REMIFENTANIL 모두 기록이 안된 Case 삭제
    
 <br>
 
- volatile (VR)
    1. 파일이 모두 결측치로 이루어진 경우 제외
    2. AGENT 변수가 기록이 안된 경우 제외
    3. 필요한 변수 추출 (PROPOFOL 제외)
    4. BIS, TV, RR의 이상치나 결측치를 마취기록지에서 대체
    5. HR 결측치 처리

## Feature Extraction
- TIVA, volatile
    1. 통계 특징 추출
    2. 변화 특징 추출
    3. 파고율 추출 
    4. 주파수 특징 추출

## Modeling
- 결과 비교하기 (MACE = TRUE, VR 빼고, etc .)
## Conclusion
