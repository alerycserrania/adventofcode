earliest = int(input())
shuttles = [int(s) for s in input().split(',') if s != 'x']
waiting_times = {s: ((earliest//s)+1)*s-earliest for s in shuttles}
min_waiting_time = min(waiting_times.values())
id_min_waiting_time = [i for i in waiting_times if waiting_times[i] == min_waiting_time][0]
print("solution:", id_min_waiting_time, min_waiting_time, id_min_waiting_time * min_waiting_time)