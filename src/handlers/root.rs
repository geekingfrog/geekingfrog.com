use std::time::Duration;


#[tracing::instrument]
pub async fn get() -> &'static str {
    println!("root handler");
    tokio::time::sleep(Duration::from_millis(100)).await;
    let result = do_stuff().await;
    tokio::time::sleep(Duration::from_millis(20)).await;
    result
}

#[tracing::instrument]
async fn do_stuff() -> &'static str {
    tokio::time::sleep(Duration::from_millis(30)).await;
    "coucou from stuff"
}
