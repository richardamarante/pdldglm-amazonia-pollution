import { useEffect, useRef, useMemo } from 'react'

function FillColour({ configState, map, layerIds }) {
	const mapRef = useRef()
	useEffect(() => {
		mapRef.current = map.current
	}, [map])

	// Get the fill colour map
	const styleFunction = useMemo(() => {
		if (!configState.choropleth) return null
		if (!configState.choropleth.fill_colour) return null

		return configState.choropleth
			? configState.choropleth.fill_colour
			: null
	}, [configState.choropleth])

	// React hook to manage change of map styling for the fill colour
	useEffect(() => {
		if (!mapRef.current || !layerIds.allLoaded) return

		layerIds.layerIds?.forEach((layerId) => {
			mapRef.current.setPaintProperty(
				layerId,
				'fill-color',
				styleFunction
			)
			mapRef.current.setPaintProperty(
				layerId,
				'fill-outline-color',
				styleFunction
			)
		})
	}, [styleFunction, layerIds])
}

export default FillColour
