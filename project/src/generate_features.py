from collections import Counter

import numpy as np
import pandas as pd


def summarise_user(user_sample, test_set=False):
    last_activity = 0

    user_activities_count = {'Clip': 0, 'Activity': 0, 'Assessment': 0, 'Game': 0}

    accuracy_groups = {0: 0, 1: 0, 2: 0, 3: 0}
    all_assessments = []
    accumulated_accuracy_group = 0
    accumulated_accuracy = 0
    accumulated_correct_attempts = 0
    accumulated_uncorrect_attempts = 0
    accumulated_actions = 0
    counter = 0
    durations = []
    last_accuracy_title = {'acc_' + title: -1 for title in assess_titles}
    event_code_count: Dict[str, int] = {ev: 0 for ev in list_of_event_code}
    event_id_count: Dict[str, int] = {eve: 0 for eve in list_of_event_id}
    title_count: Dict[str, int] = {eve: 0 for eve in activities_labels.values()}
    title_event_code_count: Dict[str, int] = {t_eve: 0 for t_eve in all_title_event_code}

    for i, session in user_sample.groupby('game_session', sort=False):
        session_type = session['type'].iloc[0]
        session_title = session['title'].iloc[0]
        session_title_text = activities_labels[session_title]

        if (session_type == 'Assessment') & (test_set or len(session) > 1):
            all_attempts = session.query(f'event_code == {win_code[session_title]}')
            true_attempts = all_attempts['event_data'].str.contains('true').sum()
            false_attempts = all_attempts['event_data'].str.contains('false').sum()

            features = user_activities_count.copy()
            features.update(last_accuracy_title.copy())
            features.update(event_code_count.copy())
            features.update(event_id_count.copy())
            features.update(title_count.copy())
            features.update(title_event_code_count.copy())
            features.update(last_accuracy_title.copy())

            features['installation_id'] = session['installation_id'].iloc[-1]
            features['session_title'] = session['title'].iloc[0]
            features['accumulated_correct_attempts'] = accumulated_correct_attempts
            features['accumulated_uncorrect_attempts'] = accumulated_uncorrect_attempts
            accumulated_correct_attempts += true_attempts
            accumulated_uncorrect_attempts += false_attempts

            if durations == []:
                features['duration_mean'] = 0
            else:
                features['duration_mean'] = np.mean(durations)
            durations.append((session.iloc[-1, 2] - session.iloc[0, 2]).seconds)
            features['accumulated_accuracy'] = accumulated_accuracy / counter if counter > 0 else 0
            accuracy = true_attempts / (true_attempts + false_attempts) if (true_attempts + false_attempts) != 0 else 0
            accumulated_accuracy += accuracy
            last_accuracy_title['acc_' + session_title_text] = accuracy
            if accuracy == 0:
                features['accuracy_group'] = 0
            elif accuracy == 1:
                features['accuracy_group'] = 3
            elif accuracy == 0.5:
                features['accuracy_group'] = 2
            else:
                features['accuracy_group'] = 1
            features.update(accuracy_groups)
            accuracy_groups[features['accuracy_group']] += 1
            features['accumulated_accuracy_group'] = accumulated_accuracy_group / counter if counter > 0 else 0
            accumulated_accuracy_group += features['accuracy_group']
            features['accumulated_actions'] = accumulated_actions

            if test_set:
                all_assessments.append(features)
            elif true_attempts + false_attempts > 0:
                all_assessments.append(features)

            counter += 1

        def update_counters(counter: dict, col: str):
            num_of_session_count = Counter(session[col])
            for k in num_of_session_count.keys():
                x = k
                if col == 'title':
                    x = activities_labels[k]
                counter[x] += num_of_session_count[k]
            return counter

        event_code_count = update_counters(event_code_count, "event_code")
        event_id_count = update_counters(event_id_count, "event_id")
        title_count = update_counters(title_count, 'title')
        title_event_code_count = update_counters(title_event_code_count, 'title_event_code')

        accumulated_actions += len(session)
        if last_activity != session_type:
            user_activities_count[session_type] += 1

    if test_set:
        return all_assessments[-1]
    return all_assessments


# Read data
train = pd.read_csv('project/input/data-science-bowl-2019/train.csv')
test = pd.read_csv('project/input/data-science-bowl-2019/test.csv')
train_labels = pd.read_csv('project/input/data-science-bowl-2019/train_labels.csv')

# Encode strings
train['title_event_code'] = list(map(lambda x, y: str(x) + '_' + str(y), train['title'], train['event_code']))
test['title_event_code'] = list(map(lambda x, y: str(x) + '_' + str(y), test['title'], test['event_code']))
all_title_event_code = list(set(train["title_event_code"].unique()).union(test["title_event_code"].unique()))

list_of_user_activities = list(set(train['title'].unique()).union(set(test['title'].unique())))
list_of_event_code = list(set(train['event_code'].unique()).union(set(test['event_code'].unique())))
list_of_event_id = list(set(train['event_id'].unique()).union(set(test['event_id'].unique())))
list_of_worlds = list(set(train['world'].unique()).union(set(test['world'].unique())))

activities_map = dict(zip(list_of_user_activities, np.arange(len(list_of_user_activities))))
activities_labels = dict(zip(np.arange(len(list_of_user_activities)), list_of_user_activities))
activities_world = dict(zip(list_of_worlds, np.arange(len(list_of_worlds))))
assess_titles = list(set(train[train['type'] == 'Assessment']['title'].value_counts().index).union(set(test[test['type'] == 'Assessment']['title'].value_counts().index)))

train['title'] = train['title'].map(activities_map)
test['title'] = test['title'].map(activities_map)
train['world'] = train['world'].map(activities_world)
test['world'] = test['world'].map(activities_world)
train_labels['title'] = train_labels['title'].map(activities_map)

win_code = dict(zip(activities_map.values(), (4100 * np.ones(len(activities_map))).astype('int')))
win_code[activities_map['Bird Measurer (Assessment)']] = 4110

train['timestamp'] = pd.to_datetime(train['timestamp'])
test['timestamp'] = pd.to_datetime(test['timestamp'])

compiled_train = []
compiled_test = []
for _, user in train.groupby('installation_id', sort=False):
    compiled_train += summarise_user(user)
for ins_id, user in test.groupby('installation_id', sort=False):
    test_data = summarise_user(user, test_set=True)
    compiled_test.append(test_data)
train_df = pd.DataFrame(compiled_train)
test_df = pd.DataFrame(compiled_test)

features = train_df.loc[(train_df.sum(axis=1) != 0), (train_df.sum(axis=0) != 0)].columns
features = [x for x in features if x not in ['accuracy_group', 'installation_id']]

train_df[features].to_csv('project/input/data-science-bowl-2019/summarised_train.csv', index=False)
test_df[features].to_csv('project/input/data-science-bowl-2019/summarised_test.csv', index=False)
