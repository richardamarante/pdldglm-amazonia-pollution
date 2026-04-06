// Update the 'click' state of the polygon that was clicked to true for styling purposes
import { useEffect, useRef } from 'react'

function HandleClickStyle({
	layerIds,
	map,
	click,
	configState,
	clickedPolygonId,
	setClickedPolygonId,
}) {
	// mapRef for reference to the map object
	const mapRef = useRef()
	useEffect(() => {
		mapRef.current = map.current
	}, [map])

	// React hook to manage click style
	useEffect(() => {
		// ensure the map object is initialized
		if (
			!mapRef.current ||
			!mapRef.current.isStyleLoaded() ||
			!configState.choropleth ||
			!configState.choropleth.pickable
		)
			return

		// Reset the 'click' state of the previously clicked polygon
		if (clickedPolygonId !== null) {
			layerIds.layerIds.forEach((layerId) => {
				mapRef.current.setFeatureState(
					{
						source: layerId,
						sourceLayer: layerId,
						id: clickedPolygonId,
					},
					{ click: false }
				)
			})
		}

		if (!click.ID) return

		layerIds.layerIds?.forEach((layerId) => {
			const features = mapRef.current.querySourceFeatures(layerId, {
				sourceLayer: [layerId],
			})
			const matchingFeature = features.find(
				(feature) => feature.properties.ID === click.ID
			)

			if (matchingFeature) {
				setClickedPolygonId(matchingFeature.id)
				mapRef.current.setFeatureState(
					{
						source: layerId,
						sourceLayer: layerId,
						id: matchingFeature.id,
					},
					{ click: true }
				)
			}
		})
	}, [
		click,
		layerIds,
		configState.choropleth,
		clickedPolygonId,
		setClickedPolygonId,
	])
}

export default HandleClickStyle
