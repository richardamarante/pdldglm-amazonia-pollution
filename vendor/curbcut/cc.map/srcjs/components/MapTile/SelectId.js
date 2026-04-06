// If there is a selected ID at init
import { useEffect, useRef } from 'react'

function SelectId({ map, select_id, layerIds, setClickedPolygonId }) {
	const mapRef = useRef()
	useEffect(() => {
		mapRef.current = map.current
	}, [map])

	useEffect(() => {
		if (!layerIds.allLoaded || !select_id) return

		const selectFeatureIfMatches = (layerId) => {
			const features = mapRef.current.querySourceFeatures(layerId, {
				sourceLayer: [layerId], // Adjust if needed
			})
			const matchingFeature = features.find(
				(feature) => feature.properties.ID === select_id
			)

			if (matchingFeature) {
				mapRef.current.setFeatureState(
					{
						source: layerId,
						sourceLayer: layerId,
						id: matchingFeature.id,
					},
					{ click: true }
				)
				setClickedPolygonId(matchingFeature.id)
			}
		}

		const onDataLoad = (event) => {
			if (event.isSourceLoaded) {
				layerIds.layerIds.forEach(selectFeatureIfMatches)
			}
		}

		mapRef.current.on('sourcedata', onDataLoad)

		return () => {
			// Cleanup the listener when component is unmounted or dependencies change
			mapRef.current.off('sourcedata', onDataLoad)
		}
	}, [layerIds, select_id, setClickedPolygonId])
}

export default SelectId
