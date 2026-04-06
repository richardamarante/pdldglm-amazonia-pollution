import { reactShinyInput } from 'reactR'

import mapboxgl from 'mapbox-gl'
import React, { useEffect, useRef, useState } from 'react'
import 'mapbox-gl/dist/mapbox-gl.css'
import NavData from './components/NavData'
import MapTile from './components/MapTile'
import GetClick from './components/GetClick'
import Stories from './components/Stories'
import PointTile from './components/PointTile'

function Map({ configuration, value, setValue }) {
	const default_style = 'mapbox://styles/curbcut/cljkciic3002h01qveq5z1wrp'
	// Set configState
	const [configState, setConfigState] = useState(() => {
		let state = Object.fromEntries(
			Object.entries(configuration).map(([key, value]) => {
				if (typeof value === 'string') {
					try {
						value = JSON.parse(value)
					} catch (e) {}
				}
				return [key, value]
			})
		)
		return state
	})

	// This effect will listen for changes in the configuration prop
	// and update the configState accordingly
	// Map over configuration to modify everything to JSON
	useEffect(() => {
		const parseConfiguration = (config, keyPath = []) => {
			return Object.entries(config).reduce((acc, [key, value]) => {
				const newKeyPath = [...keyPath, key]
				if (
					typeof value === 'object' &&
					value !== null &&
					!Array.isArray(value)
				) {
					acc[key] = parseConfiguration(value, newKeyPath)
				} else {
					if (typeof value === 'string') {
						if (key !== 'select_id') {
							try {
								value = JSON.parse(value)
							} catch (e) {}
						}
					}
					acc[key] = value
				}
				return acc
			}, {})
		}

		const parsedConfiguration = parseConfiguration(configuration)

		setConfigState((prevConfig) => {
			const updateConfig = (prev, curr, keyPath = []) => {
				return Object.entries(curr).reduce(
					(acc, [key, value]) => {
						const newKeyPath = [...keyPath, key]
						if (
							typeof value === 'object' &&
							value !== null &&
							!Array.isArray(value)
						) {
							if (!(key in acc)) acc[key] = {}
							acc[key] = updateConfig(acc[key], value, newKeyPath)
						} else {
							acc[key] = value
						}
						return acc
					},
					{ ...prev }
				)
			}

			return updateConfig(prevConfig, parsedConfiguration)
		})
	}, [configuration])

	mapboxgl.accessToken = configState.token
	const username = configState.username
	const token = configState.token

	const mapContainer = useRef(null)
	const map = useRef(null)
	const [click, setClick] = useState({
		ID: [],
		layerName: [],
		mapID: [],
	})

	// Save the initial map center and zoom. We'll use these to create the map object
	// only once, without warnings.
	const latitudeRef = useRef(configState.viewstate.latitude)
	const longitudeRef = useRef(configState.viewstate.longitude)
	const zoomRef = useRef(configState.viewstate.zoom)
	// Update the refs when the configuration changes. Ultimately, we only want to use
	// it once at init.
	useEffect(() => {
		latitudeRef.current = configState.viewstate.latitude
		longitudeRef.current = configState.viewstate.longitude
		zoomRef.current = configState.viewstate.zoom
	}, [configState.viewstate])
	// Create the map object only once, without warnings (using the refs).

	// First useEffect for map initialization
	useEffect(() => {
		console.log(`latitude: ${latitudeRef.current}`)

		map.current = new mapboxgl.Map({
			container: mapContainer.current,
			style: default_style,
			center: [Number(longitudeRef.current), Number(latitudeRef.current)],
			zoom: Number(zoomRef.current),
		})

		// Wait until the map is loaded before adding any controls or performing any other operations
		map.current.on('load', () => {
			map.current.addControl(
				new mapboxgl.NavigationControl(),
				'bottom-right'
			)
		})

		const resizeObserver = new ResizeObserver(() => {
			map.current.resize()
		})

		resizeObserver.observe(mapContainer.current)

		return () => {
			map.current.remove()
			resizeObserver.disconnect()
		}
	}, [])

	// Second useEffect for handling style updates
	useEffect(() => {
		if (!map.current) return
		if (!configState.style) return
		if (configState.style === default_style) return
		map.current.setStyle(configState.style)
	}, [configState.style])

	// Third useEffect for handling viewstate updates
	useEffect(() => {
		if (!map.current) return

		// Update map center and zoom
		map.current.setCenter([
			configState.viewstate.longitude,
			configState.viewstate.latitude,
		])
		map.current.setZoom(configState.viewstate.zoom)
	}, [configState.viewstate])

	// Add/update the source layers to the map
	MapTile({
		map,
		configState,
		click,
		username,
		token,
		setClick,
	})

	PointTile({ map, configState, username, token })

	// Update the coordinates to the shiny input
	NavData({ map, setValue })

	// Save the ID of the clicked tileset to the `click` state
	GetClick({ map, click, setClick, setValue, configState })

	// Add stories icons to the map
	Stories({ map, configState, username })

	return (
		<div
			ref={mapContainer}
			className='map-container'
			style={{ height: '100%', width: '100%' }}
		/>
	)
}

reactShinyInput('.map', 'cc.map.map', Map)
