export const fetchTextImpl = (url) => (onError, onSuccess) => {
  const controller = new AbortController()

  fetch(url, { signal: controller.signal })
    .then((response) =>
      response.ok
        ? response.text()
        : Promise.reject(new Error(url + ": " + response.status + " " + response.statusText))
    )
    .then(onSuccess)
    .catch(onError)

  return (_cancelError, _onCancelerError, onCancelerSuccess) => {
    controller.abort()
    onCancelerSuccess()
  }
}
