import { useEffect } from 'react'

function LayerJson({ setSourceLayers, username, tileset, token }) {
	useEffect(() => {
		if (!tileset) return
		if (tileset === 'remove') {
			setSourceLayers({ vector_layers: [], url: '' })
			return
		}

		const layerUrl = `https://api.mapbox.com/v4/${username}.${tileset}.json?secure&access_token=${token}`
		fetch(layerUrl)
			.then((response) => response.json())
			.then((srcLayers) => {
				const url = `mapbox://${username}.${tileset}`
				setSourceLayers({
					vector_layers: srcLayers?.vector_layers,
					url: url,
				})
			})
			.catch((error) => console.error('Error:', error))
	}, [username, tileset, token, setSourceLayers])

	return null
}

export default LayerJson
