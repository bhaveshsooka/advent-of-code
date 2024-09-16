use reqwest::header::COOKIE;

pub async fn get_question_data(year: i32, day: i32) -> String {
    let url: String = format!("https://adventofcode.com/{year}/day/{day}/input");
    let cookie: String = std::env::var("COOKIE").expect("COOKIE must be set.");

    reqwest::Client::new()
        .get(url)
        .header(COOKIE, cookie)
        .send()
        .await
        .expect("Error sending request to get input")
        .text()
        .await
        .expect(format!("Could not get input data for year: {year}, day: {day}").as_str())
}
