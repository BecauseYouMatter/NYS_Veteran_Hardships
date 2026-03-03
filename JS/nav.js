const pages = [
  "index.htm",
  "NYS_Vet01.htm",
  "NYS_Vet02.htm",
  "NYS_Vet03.htm",
  "NYS_Vet04.htm"
];

function currentPageIndex() {
  const path = window.location.pathname;
  const fileName = path.substring(path.lastIndexOf("/") + 1);
  return pages.indexOf(fileName);
}

function goBack() {
  const v = currentPageIndex();
  if (v > 0) {
    window.location.href = pages[v - 1];
  }
}

function goNext() {
  const v = currentPageIndex();
  if (v >= 0 && v < pages.length - 1) {
    window.location.href = pages[v + 1];
  }
}

function goDownloads() {
  const v = currentPageIndex();
  window.location.href = "downloads.htm?from=" + v;
}

function getFromPage() {
  const params = new URLSearchParams(window.location.search);
  const from = parseInt(params.get("from"), 10);
  return isNaN(from) ? 0 : from;
}

function goBackFromDownloads() {
  const from = getFromPage();
  window.location.href = pages[from];
}

function goNextFromDownloads() {
  const from = getFromPage();
  if (from < pages.length - 1) {
    window.location.href = pages[from + 1];
  }
}