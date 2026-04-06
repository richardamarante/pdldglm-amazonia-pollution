import { useState, useEffect, useRef } from 'react'

function BuildingStyle({ sourceLayers, map }) {
	const mapRef = useRef()
	useEffect(() => {
		mapRef.current = map.current
	}, [map])

	// Add zoom as a state
	const [zoom, setZoom] = useState(0)

	useEffect(() => {
		if (!mapRef.current) return

		const handleMove = () => {
			const zoom = mapRef.current.getZoom()
			setZoom(zoom)
		}

		mapRef.current.on('moveend', handleMove)

		return () => {
			mapRef.current.off('moveend', handleMove)
		}
	}, [])

	// React hook to manage building layer
	useEffect(() => {
		if (!mapRef.current) return
		if (!sourceLayers.vector_layers) return
		if (sourceLayers.vector_layers.length === 0) return
		if (!mapRef.current.isStyleLoaded()) return

		const layers = mapRef.current.getStyle().layers
		const buildingLayer = layers.find(
			(layer) => layer.type === 'fill' && layer.id === 'building'
		)
		if (!buildingLayer) return

		const buildingLayerId = buildingLayer.id

		console.log(zoom)

		const visibleBuildingSourceLayer = sourceLayers.vector_layers.find(
			(layer) =>
				layer.id.includes('building') &&
				zoom >= layer.minzoom &&
				zoom <= layer.maxzoom
		)

		console.log(visibleBuildingSourceLayer)

		if (visibleBuildingSourceLayer) {
			mapRef.current.setLayoutProperty(
				buildingLayerId,
				'visibility',
				'none'
			)
		} else {
			mapRef.current.setLayoutProperty(
				buildingLayerId,
				'visibility',
				'visible'
			)
		}
	}, [zoom, sourceLayers.vector_layers])
}
export default BuildingStyle
