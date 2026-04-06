import { useEffect, useCallback } from 'react'

function GetClick({ map, click, setClick, setValue, configState }) {
	const currentMap = map.current

	const stableSetValue = useCallback(setValue, [])

	// React hook to manage click
	useEffect(() => {
		if (!currentMap) return
		const onClick = function (e) {
			var features = currentMap.queryRenderedFeatures(e.point)
			var notSimpleFeatures = features.filter((f) => {
				if (f.layer?.metadata) {
					const metadataKeys = Object.keys(f.layer.metadata)
					return metadataKeys.every(
						(key) => !key.startsWith('mapbox:')
					)
				}
				if (f.layer.id === 'building') return false
				return true
			})

			const ID = notSimpleFeatures[0]?.properties.ID
			const layerName = notSimpleFeatures[0]?.layer['source-layer']

			if (click.ID === ID) {
				setClick({
					ID: [],
					layerName: [],
				})
				stableSetValue({
					ID: [],
					layerName: [],
					event: 'click',
				})
			} else {
				setClick({
					ID,
					layerName,
				})
				stableSetValue({
					ID,
					layerName,
					event: 'click',
				})
			}
		}

		currentMap.on('click', onClick)

		return () => {
			currentMap.off('click', onClick)
		}
	}, [setClick, click, currentMap, stableSetValue])

	// React hook to manage when configState includes a selection
	useEffect(() => {
		if (!currentMap) return // wait for map to initialize
		if (!configState.selection) return

		setClick({
			ID: configState.selection.select_id,
		})
	}, [configState.selection, setClick, currentMap])

	// If we want to inject a selection on the map
	useEffect(() => {
		if (!currentMap) return // wait for map to initialize

		setClick({
			ID: configState.select_id,
		})
	}, [configState.select_id, currentMap, setClick])

	return null
}

export default GetClick
