import { useEffect, useRef } from 'react'

function HandleFilter({ map, configState, layerIds }) {
	const mapRef = useRef()
	useEffect(() => {
		mapRef.current = map.current
	}, [map])

	useEffect(() => {
		if (!mapRef.current) return
		if (!configState.heatmap) return
		if (!configState.heatmap.filter) return

		layerIds.layerIds?.forEach((layerId) => {
			mapRef.current.setFilter(layerId, configState.heatmap.filter)
			mapRef.current.setFilter(
				layerId + '-point',
				configState.heatmap.filter
			)
		})
	}, [map, configState.heatmap, layerIds])
}

export default HandleFilter
