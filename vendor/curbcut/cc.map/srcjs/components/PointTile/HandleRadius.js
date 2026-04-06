import { useEffect, useRef } from 'react'

function HandleRadius({ map, configState, layerIds }) {
	const mapRef = useRef()
	useEffect(() => {
		mapRef.current = map.current
	}, [map])

	useEffect(() => {
		if (!mapRef.current) return
		if (!configState.heatmap) return
		if (!configState.heatmap.filter) return

		layerIds.layerIds?.forEach((layerId) => {
			mapRef.current.setPaintProperty(
				layerId,
				'heatmap-radius',
				configState.heatmap.radius
			)
		})
	}, [mapRef, configState.heatmap, layerIds])
}

export default HandleRadius
