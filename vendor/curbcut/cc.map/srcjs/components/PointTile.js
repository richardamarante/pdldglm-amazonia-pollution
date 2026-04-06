import { useEffect, useRef, useState, useMemo } from 'react'
import LayerJson from './LayerJson'
import HandleFilter from './PointTile/HandleFilter'
import HandleRadius from './PointTile/HandleRadius'

function PointTile({ map, configState, username, token }) {
	const layerIdsRef = useRef([])
	const mapRef = useRef()

	useEffect(() => {
		mapRef.current = map.current
	}, [map])

	const tileset = useMemo(() => {
		if (!configState.heatmap) return null
		if (!configState.heatmap.tileset) return null

		return configState.heatmap ? configState.heatmap.tileset : null
	}, [configState.heatmap])
	const pickable = useMemo(() => {
		if (!configState.heatmap) return null
		if (!configState.heatmap.pickable) return null

		return configState.heatmap ? configState.heatmap.pickable : false
	}, [configState.heatmap])

	// Load the sourceLayers depending on configState.tileset
	const [sourceLayers, setSourceLayers] = useState({
		vector_layers: [],
		url: '',
	})
	// Get the source layers in the active tileset
	LayerJson({
		setSourceLayers,
		username,
		tileset: tileset,
		token,
	})

	// Keep current loaded layer IDs
	const [layerIds, setLayerIds] = useState({ layerIds: [], allLoaded: false })

	useEffect(() => {
		const handleLoad = () => {
			const layers = mapRef.current.getStyle().layers
			const buildingLayerId = layers.find(
				(layer) => layer.type === 'symbol' && layer.id.includes('label')
			).id

			// Keep track of added layers
			const layerIds = []
			let hoveredPointId = null

			// Add the source layers to the map
			sourceLayers.vector_layers?.forEach((sourceLayer, index) => {
				const layerId = sourceLayer.id
				setLayerIds((prevLayerIds) => ({
					layerIds: [...prevLayerIds.layerIds, layerId],
					allLoaded: false,
				}))
				mapRef.current.addSource(layerId, {
					type: 'vector',
					url: sourceLayers.url,
				})

				// Add the heatmap layer
				mapRef.current.addLayer(
					{
						id: layerId,
						type: 'heatmap',
						source: layerId,
						'source-layer': sourceLayer.id,
						minzoom: sourceLayer.minzoom,
						maxzoom: sourceLayer.maxzoom,
						paint: {
							// Color ramp for heatmap.  Domain is 0 (low) to 1 (high).
							// Begin color ramp at 0-stop with a 0-transparancy color
							// to create a blur-like effect.
							'heatmap-color': [
								'interpolate',
								['linear'],
								['heatmap-density'],
								0,
								configState.heatmap.colours[0],
								0.25,
								configState.heatmap.colours[1],
								0.5,
								configState.heatmap.colours[2],
								0.75,
								configState.heatmap.colours[3],
								1,
								configState.heatmap.colours[4],
							],
							// Transition from heatmap to circle layer by zoom level
							'heatmap-opacity': [
								'interpolate',
								['linear'],
								['zoom'],
								15,
								1,
								16,
								0,
							],
						},
					},
					buildingLayerId
				)

				// Prepare the rgb colours[4] to RGBA (with the alpha)
				let rgb = configState.heatmap.colours[4]
				let rgba = rgb.replace('rgb', 'rgba').replace(')', ', 0.5)')

				// Add a layer with the points
				mapRef.current.addLayer(
					{
						id: layerId + '-point',
						type: 'circle',
						source: layerId,
						'source-layer': sourceLayer.id,
						minzoom: configState.heatmap.minzoom,
						maxzoom: sourceLayer.maxzoom,
						paint: {
							// Increase the radius of the circle as the zoom level and dbh value increases
							'circle-radius': [
								'interpolate',
								['linear'],
								['zoom'],
								10,
								0,
								12,
								1,
								22,
								15,
							],
							'circle-color': [
								'case',
								['boolean', ['feature-state', 'hover'], false],
								configState.heatmap.colours[4],
								rgba,
							],
							'circle-stroke-color':
								configState.heatmap.strokeColor,
							'circle-stroke-width': 1,
							'circle-opacity': [
								'interpolate',
								['linear'],
								['zoom'],
								15,
								0,
								16,
								1,
							],
						},
					},
					'road-label-simple'
				)

				// Add the layer id to our array of added layers
				layerIdsRef.current = layerIds

				// If the layer is not pickable, then we don't want to add the hover effect
				if (!pickable) return

				// On the layer, set the feature state to `hover: true` when the mouse
				// is over it.
				mapRef.current.on('mousemove', layerId, (e) => {
					if (e.features.length > 0) {
						if (hoveredPointId !== null) {
							mapRef.current.setFeatureState(
								{
									source: layerId,
									sourceLayer: sourceLayer.id,
									id: hoveredPointId,
								},
								{ hover: false }
							)
						}
						hoveredPointId = e.features[0].id
						mapRef.current.setFeatureState(
							{
								source: layerId,
								sourceLayer: sourceLayer.id,
								id: hoveredPointId,
							},
							{ hover: true }
						)
					}
				})

				// When the mouse leaves the layer, update the feature state of the
				// previously hovered feature.
				mapRef.current.on('mouseleave', layerId, () => {
					if (hoveredPointId !== null) {
						mapRef.current.setFeatureState(
							{
								source: layerId,
								sourceLayer: sourceLayer.id,
								id: hoveredPointId,
							},
							{ hover: false }
						)
					}
					hoveredPointId = null
				})
			})

			// Once all the layers are loaded
			setLayerIds((prevState) => ({
				...prevState,
				layerIds: [...prevState.layerIds], // add the layer id
				allLoaded: true,
			}))
		}

		// This function will clean up (remove) layers added from previous runs of this effect
		const removeLayers = () => {
			const currentLayerIds = [...layerIds.layerIds] // Make a shallow copy

			currentLayerIds.forEach((layerId) => {
				if (mapRef.current.getLayer(layerId)) {
					mapRef.current.off('mousemove', layerId)
					mapRef.current.off('mouseleave', layerId)
					mapRef.current.removeLayer(layerId + '-point')
					mapRef.current.removeLayer(layerId)
					mapRef.current.removeSource(layerId)
				}
			})

			// Clear the ref after removing layers
			setLayerIds({ layerIds: [], allLoaded: false })
		}

		removeLayers() // Remove existing layers first

		// Add new layers afterwards
		if (mapRef.current.isStyleLoaded()) {
			handleLoad()
		} else {
			mapRef.current.on('load', handleLoad)
		}

		// Cleanup function to run when component is unmounted or when dependencies change
		return () => {
			mapRef.current.off('load')
			removeLayers() // Remove existing layers
		}
	}, [sourceLayers, pickable, setLayerIds])

	HandleFilter({ map, configState, layerIds })
	HandleRadius({ map, configState, layerIds })
}
export default PointTile
