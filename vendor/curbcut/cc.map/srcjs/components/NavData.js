import { useEffect, useRef, useCallback } from 'react'

function NavData({ map, setValue }) {
	const mapRef = useRef()

	const stableSetValue = useCallback(setValue, [])

	useEffect(() => {
		mapRef.current = map.current
	}, [map]) // sync mapRef.current with map.current

	useEffect(() => {
		if (!mapRef.current) return // wait for map to initialize

		const handleMove = () => {
			const bounds = mapRef.current.getBounds()
			const sw = bounds.getSouthWest()
			const ne = bounds.getNorthEast()

			const lon = mapRef.current.getCenter().lng.toFixed(4)
			const lat = mapRef.current.getCenter().lat.toFixed(4)
			const zoom = mapRef.current.getZoom().toFixed(2)

			stableSetValue({
				longitude: lon,
				latitude: lat,
				zoom: zoom,
				event: 'viewstate',
				boundingbox: {
					southWest: {
						lat: sw.lat.toFixed(4),
						lon: sw.lng.toFixed(4),
					},
					northEast: {
						lat: ne.lat.toFixed(4),
						lon: ne.lng.toFixed(4),
					},
				},
			})
		}

		mapRef.current.on('moveend', handleMove)

		return () => {
			mapRef.current.off('moveend', handleMove)
		}
	}, [stableSetValue]) // use stableSetValue as a dependency

	return null
}

export default NavData
