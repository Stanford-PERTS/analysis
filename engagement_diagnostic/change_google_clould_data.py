# in this file I create queries, where I can change the filename of 
# the html version of the existing reports
# The files were previously saved with their conventional names,
# so the script relies that Team and Classroom information can be inferred from
# filename. Note that classroom and team queries are different, so you should be able to 
# distinguish between them, so if the information is not in the filename, make 
# sure you have an alternative source.

import glob, os, re
# os.getcwd()

file_names = []
os.chdir('/Users/rumen/Desktop/date_fix')
for file in glob.glob("*.html"):
    file_names.append(file)
    #print(file)

date_file_name = '2019-02-18.html'

query_text_team = """UPDATE report
SET gcs_path = 'file_name_id'
WHERE filename = 'date_file_name'
AND team_id = 'Team_id'
AND type = 'Type_id';"""
query_text_classroom = re.sub("AND team_id = 'Team_id'\\n",'',query_text_team)
for file_name in file_names:
    ru = re.sub('\\.' + date_file_name,'',file_name)
    classroom_id = ru.find("Classroom")
    team_id = ru.find("Team")
    query_text = ""
    if team_id == 0:
        query_text = re.sub("Team_id", ru, query_text_team)
        query_text = re.sub("Type_id", "Team", query_text)
    if classroom_id == 0:
        query_text = re.sub("Type_id", ru, query_text_classroom)
    query_text = re.sub("date_file_name", date_file_name, query_text)
    query_text = re.sub("file_name_id", '/tritonplatform-upload/' + file_name, query_text)
    print
    print(query_text)

