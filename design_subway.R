# 시간 제한 해제
options(deviceTimeout = Inf)
# RStudio 그래픽 디바이스 시간 제한 해제
Sys.setenv("_R_CHECK_DEVICE_TIMEOUT_" = "off")

if(!require('ggmap')) install.packages("ggmap");
if(!require('dplyr')) install.packages("dplyr");
if (!require("igraph")) install.packages("igraph");
if (!require("geosphere")) install.packages("geosphere");
library(ggmap)
library(dplyr)
library(igraph)
library(geosphere)  # 거리 계산을 위한 패키지

# Google Maps API 키 등록
register_google(key = "YOUR API KEY VALUE")

#---------------------------------------------------------------------------------------
# 1호선 데이터 하드코딩
line1_data <- data.frame(
  역위도 = c(35.10703, 35.12411, 35.13302, 35.13840, 35.14661, 35.15098, 35.15318,
          35.15418, 35.15205, 35.15356, 35.15250, 35.15179, 35.15049, 35.14678,
          35.14321, 35.14418, 35.14357, 35.13753, 35.13171, 35.12470),
  역경도 = c(126.9339, 126.9320, 126.9284, 126.9218, 126.9201, 126.9144, 126.9109,
          126.9045, 126.8956, 126.8840, 126.8777, 126.8695, 126.8587, 126.8486,
          126.8414, 126.8115, 126.7992, 126.7915, 126.7879, 126.7696)
)

# 1호선 데이터 소수점 6자리로 반올림
line1_data <- line1_data %>%
  mutate(
    역위도 = round(역위도, 6),
    역경도 = round(역경도, 6)
  )

#---------------------------------------------------------------------------------------
# 버스 정류장 데이터 처리
# 데이터 불러오기
bus_data <- read.csv("gwangju_bus_cus_2022.csv", stringsAsFactors = FALSE)
location_data <- read.csv("gwangju_bus_loc.csv", stringsAsFactors = FALSE)

# 버스 정류장 번호를 기준으로 병합
merged_data <- bus_data %>%
  rename(정류장번호 = 정류장.번호) %>%
  group_by(정류장번호, 권종) %>%
  summarize(total_transactions = sum(거래건수, na.rm = TRUE), .groups = "drop") %>%
  left_join(location_data, by = c("정류장번호" = "정류장번호"))

# 결측값 제거
merged_data <- merged_data %>%
  filter(!is.na(경도) & !is.na(위도))

# 거래 건수 상위 30개 필터링
top_stations <- merged_data %>%
  arrange(desc(total_transactions)) %>%
  slice_head(n = 30)

#---------------------------------------------------------------------------------------
# 네트워크 그래프 생성
# 모든 노드 간 거리 계산
edges <- expand.grid(
  from = top_stations$정류장번호,
  to = top_stations$정류장번호
) %>%
  filter(from != to) %>%
  mutate(
    weight = distHaversine(
      cbind(top_stations$경도[match(from, top_stations$정류장번호)],
            top_stations$위도[match(from, top_stations$정류장번호)]),
      cbind(top_stations$경도[match(to, top_stations$정류장번호)],
            top_stations$위도[match(to, top_stations$정류장번호)])
    )
  )

# 그래프 생성
bus_graph <- graph_from_data_frame(edges, directed = FALSE)

#---------------------------------------------------------------------------------------
# 근접중심성 기반 2호선 정류장 선정
closeness_scores <- closeness(bus_graph, normalized = FALSE)

# 근접중심성 상위 10개 정류장 선택
top_closeness <- top_stations %>%
  mutate(closeness = closeness_scores[as.character(정류장번호)]) %>%
  arrange(desc(closeness)) %>%
  slice_head(n = 10)
print(top_closeness)
#---------------------------------------------------------------------------------------
# 네트워크 그래프 생성
# 모든 노드 간 거리 계산
# 모든 가중치를 양수로 변환
edges <- edges %>%
  mutate(weight = weight - min(weight) + 1)

# 그래프 다시 생성
bus_graph <- graph_from_data_frame(edges, directed = FALSE)
#---------------------------------------------------------------------------------------
# 엣지 데이터 확인(데이터 검증용 코드드)
# head(edges)
# 그래프에서 연결 컴포넌트 확인
# components(bus_graph)$no  # 연결된 컴포넌트 개수
# summary(edges$weight)
#---------------------------------------------------------------------------------------

# 중개중심성 기반 2호선 정류장 선정
betweenness_scores <- betweenness(bus_graph, normalized = FALSE)

# 중개중심성 상위 10개 정류장 선택
top_betweenness <- top_stations %>%
  mutate(betweenness = betweenness_scores[as.character(정류장번호)]) %>%
  arrange(desc(betweenness)) %>%
  slice_head(n = 10)
print(top_betweenness)
#---------------------------------------------------------------------------------------
# 지도 가져오기
gwangju_map <- get_map(location = c(lon = 126.85, lat = 35.15), zoom = 12, maptype = "roadmap")

#---------------------------------------------------------------------------------------
full_plot <- ggmap(gwangju_map) +
  # 1호선 경로
  geom_path(
    data = line1_data,
    aes(x = 역경도, y = 역위도),
    color = "red", linewidth = 1.5, alpha = 0.8
  ) +
  geom_point(
    data = line1_data,
    aes(x = 역경도, y = 역위도),
    color = "darkred", size = 2
  ) +
  # 상위 30개 버스 정류장
  geom_point(
    data = top_stations,
    aes(x = 경도, y = 위도, size = total_transactions, color = 권종),
    alpha = 0.7
  )

#---------------------------------------------------------------------------------------
# 근접중심성 기반 지도 생성
closeness_plot <- ggmap(gwangju_map) +
  # 1호선 경로
  geom_path(
    data = line1_data,
    aes(x = 역경도, y = 역위도),
    color = "red", linewidth = 1.5, alpha = 0.8
  ) +
  geom_point(
    data = line1_data,
    aes(x = 역경도, y = 역위도),
    color = "darkred", size = 2
  ) +
  # 상위 30개 버스 정류장
  geom_point(
    data = top_stations,
    aes(x = 경도, y = 위도, size = total_transactions, color = 권종),
    alpha = 0.7
  ) +
  # 근접중심성 기반 2호선 경로
  geom_point(
    data = top_closeness,
    aes(x = 경도, y = 위도),
    color = "black", size = 3
  ) +
  geom_path(
    data = top_closeness %>% arrange(경도, 위도),
    aes(x = 경도, y = 위도),
    color = "black", linewidth = 1, alpha = 0.9
  ) +
  scale_color_manual(values = c("어린이" = "blue", "일반" = "green", "청소년" = "orange")) +
  labs(title = "광주 근접중심성을 고려한 2호선 경로", size = "거래 건수", color = "권종") +
  theme_minimal()

#---------------------------------------------------------------------------------------
# 중개중심성 기반 지도 생성
betweenness_plot <- ggmap(gwangju_map) +
  # 1호선 경로
  geom_path(
    data = line1_data,
    aes(x = 역경도, y = 역위도),
    color = "red", linewidth = 1.5, alpha = 0.8
  ) +
  geom_point(
    data = line1_data,
    aes(x = 역경도, y = 역위도),
    color = "darkred", size = 2
  ) +
  # 상위 30개 버스 정류장
  geom_point(
    data = top_stations,
    aes(x = 경도, y = 위도, size = total_transactions, color = 권종),
    alpha = 0.7
  ) +
  # 중개중심성 기반 2호선 경로
  geom_point(
    data = top_betweenness,
    aes(x = 경도, y = 위도),
    color = "black", size = 3
  ) +
  geom_path(
    data = top_betweenness %>% arrange(경도, 위도),
    aes(x = 경도, y = 위도),
    color = "black", linewidth = 1, alpha = 0.9
  ) +
  scale_color_manual(values = c("어린이" = "blue", "일반" = "green", "청소년" = "orange")) +
  labs(title = "광주 중개중심성을 고려한 2호선 경로", size = "거래 건수", color = "권종") +
  theme_minimal()

#---------------------------------------------------------------------------------------
# 결과 출력 및 저장

print(full_plot)
ggsave("top30버스승하차와1호선.png", plot = full_plot, width = 12, height = 8)

print(closeness_plot)
ggsave("근접중심성기반_2호선.png", plot = closeness_plot, width = 12, height = 8)

print(betweenness_plot)
ggsave("매개중심성기반_2호선.png", plot = betweenness_plot, width = 12, height = 8)
