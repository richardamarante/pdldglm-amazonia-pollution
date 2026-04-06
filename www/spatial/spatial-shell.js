(function () {
  var mapRegistry = window.ic2025SpatialMaps || (window.ic2025SpatialMaps = {});
  var timelineDragState = null;

  function resizeSpatialMaps() {
    Object.keys(mapRegistry).forEach(function (id) {
      var state = mapRegistry[id];
      if (state && state.map) {
        try {
          state.map.resize();
        } catch (e) {}
      }
    });
  }

  function triggerResize() {
    window.dispatchEvent(new Event("resize"));
    resizeSpatialMaps();
  }

  function markSpatialBootReady(state) {
    if (!state || !state.root || state.bootReady) {
      return;
    }
    state.bootReady = true;
    state.root.classList.remove("is-booting");
    state.root.setAttribute("data-spatial-ready", "1");
  }

  function setSpatialShell(active) {
    document.body.classList.toggle("ic2025-spatial-mode", !!active);
    setTimeout(triggerResize, 40);
    setTimeout(triggerResize, 180);
    setTimeout(triggerResize, 420);
  }

  function cleanupIntroJsArtifacts() {
    try {
      if (window.introJs && typeof window.introJs === "function") {
        var intro = window.introJs();
        if (intro && typeof intro.exit === "function") {
          intro.exit();
        }
      }
    } catch (e) {}
    [
      ".introjs-overlay",
      ".introjs-helperLayer",
      ".introjs-tooltipReferenceLayer",
      ".introjs-disableInteraction",
      ".introjs-showElement"
    ].forEach(function (selector) {
      document.querySelectorAll(selector).forEach(function (node) {
        if (selector === ".introjs-showElement") {
          node.classList.remove("introjs-showElement");
          return;
        }
        node.remove();
      });
    });
    document.body.classList.remove("introjs-open");
  }

  function storageAvailable() {
    try {
      var key = "__ic2025_spatial_probe__";
      window.localStorage.setItem(key, key);
      window.localStorage.removeItem(key);
      return true;
    } catch (e) {
      return false;
    }
  }

  function readStorageJson(key) {
    if (!storageAvailable() || !key) {
      return null;
    }
    try {
      var raw = window.localStorage.getItem(key);
      return raw ? JSON.parse(raw) : null;
    } catch (e) {
      return null;
    }
  }

  function writeStorageJson(key, value) {
    if (!storageAvailable() || !key) {
      return;
    }
    try {
      window.localStorage.setItem(key, JSON.stringify(value || {}));
    } catch (e) {}
  }

  function removeStorageKey(key) {
    if (!storageAvailable() || !key) {
      return;
    }
    try {
      window.localStorage.removeItem(key);
    } catch (e) {}
  }

  function syncSpatialGroupTabs(root) {
    var scope = root || document;
    var select = scope.querySelector('.ic2025-spatial-group-tabs select');
    var tabs = scope.querySelectorAll('.ic2025-spatial-group-tab');
    if (!select || !tabs.length) {
      return;
    }
    var value = select.value || "";
    tabs.forEach(function (tab) {
      var isActive = tab.getAttribute("data-group") === value;
      tab.classList.toggle("is-active", isActive);
    });
  }

  function installSpatialGroupTabs() {
    if (window.__ic2025SpatialGroupTabsInstalled) {
      syncSpatialGroupTabs(document);
      return;
    }
    if (!document.body) {
      window.setTimeout(installSpatialGroupTabs, 50);
      return;
    }
    window.__ic2025SpatialGroupTabsInstalled = true;

    document.addEventListener("click", function (event) {
      var button = event.target.closest(".ic2025-spatial-group-tab");
      if (!button) {
        return;
      }
      var wrap = button.closest(".ic2025-spatial-group-tabs");
      var select = wrap ? wrap.querySelector("select") : null;
      var groupValue = button.getAttribute("data-group") || "";
      if (!select || !groupValue) {
        return;
      }
      if (select.value !== groupValue) {
        select.value = groupValue;
        select.dispatchEvent(new Event("change", { bubbles: true }));
      }
      syncSpatialGroupTabs(wrap);
    });

    document.addEventListener("change", function (event) {
      if (event.target.closest(".ic2025-spatial-group-tabs")) {
        syncSpatialGroupTabs(event.target.closest(".ic2025-spatial-group-tabs"));
      }
    });

    var observer = new MutationObserver(function () {
      syncSpatialGroupTabs(document);
    });
    observer.observe(document.body, {
      subtree: true,
      childList: true,
      attributes: true,
      attributeFilter: ["value", "class"]
    });

    syncSpatialGroupTabs(document);
  }

  function syncShellFromSidebar() {
    var activeLink = document.querySelector('.sidebar-menu li.active a[data-value]');
    var activeValue = activeLink ? activeLink.getAttribute("data-value") : "";
    setSpatialShell(activeValue === "spatial");
  }

  function installSidebarObserver() {
    var menu = document.querySelector(".sidebar-menu");
    if (!menu) {
      window.setTimeout(installSidebarObserver, 120);
      return;
    }

    if (window.__ic2025SpatialSidebarObserverInstalled) {
      return;
    }
    window.__ic2025SpatialSidebarObserverInstalled = true;

    var observer = new MutationObserver(function () {
      syncShellFromSidebar();
    });

    observer.observe(menu, {
      attributes: true,
      subtree: true,
      attributeFilter: ["class"]
    });
  }

  function installShellPoller() {
    if (window.__ic2025SpatialShellPollerInstalled) {
      return;
    }
    window.__ic2025SpatialShellPollerInstalled = true;
    window.setInterval(syncShellFromSidebar, 500);
  }

  function ensureMapboxReady(callback, attempt) {
    var tries = attempt || 0;
    if (window.mapboxgl && typeof window.mapboxgl.Map === "function") {
      callback();
      return;
    }
    if (tries > 80) {
      return;
    }
    window.setTimeout(function () {
      ensureMapboxReady(callback, tries + 1);
    }, 80);
  }

  function buildBounds(bbox) {
    if (!bbox) {
      return null;
    }
    return [
      [Number(bbox.xmin), Number(bbox.ymin)],
      [Number(bbox.xmax), Number(bbox.ymax)]
    ];
  }

  function buildMatchExpression(items, propertyName, fieldName, fallback) {
    var expr = ["match", ["to-string", ["get", propertyName]]];
    (items || []).forEach(function (item) {
      if (!item || item.key == null) {
        return;
      }
      expr.push(String(item.key));
      expr.push(item[fieldName] == null ? fallback : item[fieldName]);
    });
    expr.push(fallback);
    return expr;
  }

  function buildVisibleFilter(propertyName, keys) {
    if (!Array.isArray(keys)) {
      return ["has", propertyName];
    }
    if (keys.length === 0) {
      return propertyName === "abbrev_state"
        ? ["has", propertyName]
        : ["==", ["literal", 1], 0];
    }
    return ["in", ["to-string", ["get", propertyName]], ["literal", keys.map(String)]];
  }

  function popupHtml(meta, metricLabel) {
    if (!meta) {
      return "";
    }
    return (
      "<div class='ic2025-spatial-popup'>" +
      "<div class='ic2025-spatial-popup-kicker'>Explore</div>" +
      "<div class='ic2025-spatial-popup-title'>" + (meta.placeLabel || "") + "</div>" +
      "<div class='ic2025-spatial-popup-body'>" +
      (metricLabel || "") + ": " + (meta.valueLabel || "Sem dado") +
      "</div>" +
      "</div>"
    );
  }

  function clampNumber(value, minValue, maxValue) {
    return Math.min(Math.max(value, minValue), maxValue);
  }

  function shouldHideBaseRoadLayer(layer) {
    if (!layer || layer.type !== "line" || !layer.id) {
      return false;
    }
    var id = String(layer.id || "").toLowerCase();
    if (/admin|boundary|water|waterway|river|lake|coast|shore|rail|ferry|building|contour|hillshade|landuse|park/i.test(id)) {
      return false;
    }
    return /road|street|motorway|highway|traffic|transport|bridge|tunnel|link|path|track|pedestrian|service/i.test(id);
  }

  function hideBaseRoads(map) {
    return;
  }

  function shouldHideBaseHydrologyLayer(layer) {
    if (!layer || !layer.id || layer.type !== "line") {
      return false;
    }
    var id = String(layer.id || "").toLowerCase();
    if (/admin|boundary|building|landuse|park|hillshade|contour|rail|road|street|motorway|highway|traffic|transport|coast|shore|ocean|sea|marine/i.test(id)) {
      return false;
    }
    return /waterway|river|stream|canal|dam|hydro|lake|reservoir/i.test(id);
  }

  function hideBaseHydrology(map) {
    return;
  }

  function strengthenBaseBoundaries(map) {
    if (!map || typeof map.getLayer !== "function" || typeof map.setPaintProperty !== "function") {
      return;
    }
    try {
      if (map.getLayer("admin-1-boundary-bg")) {
        map.setPaintProperty("admin-1-boundary-bg", "line-width", [
          "interpolate",
          ["linear"],
          ["zoom"],
          3, 3.8,
          12, 7.2
        ]);
        map.setPaintProperty("admin-1-boundary-bg", "line-opacity", [
          "interpolate",
          ["linear"],
          ["zoom"],
          6.5, 0.12,
          8, 0.62
        ]);
        map.setPaintProperty("admin-1-boundary-bg", "line-blur", [
          "interpolate",
          ["linear"],
          ["zoom"],
          3, 0.2,
          12, 2.4
        ]);
      }
      if (map.getLayer("admin-1-boundary")) {
        map.setPaintProperty("admin-1-boundary", "line-width", [
          "interpolate",
          ["linear"],
          ["zoom"],
          3, 0.7,
          12, 2.2
        ]);
        map.setPaintProperty("admin-1-boundary", "line-opacity", 0.96);
        map.setPaintProperty(
          "admin-1-boundary",
          "line-color",
          themeColor("spatial.border_medium") || "rgba(88, 93, 108, 0.92)"
        );
        map.setPaintProperty("admin-1-boundary", "line-dasharray", ["literal", [1, 0]]);
      }
      if (map.getLayer("admin-0-boundary-bg")) {
        map.setPaintProperty("admin-0-boundary-bg", "line-opacity", [
          "interpolate",
          ["linear"],
          ["zoom"],
          3, 0.14,
          6, 0.52
        ]);
      }
      if (map.getLayer("admin-0-boundary")) {
        map.setPaintProperty(
          "admin-0-boundary",
          "line-color",
          themeColor("spatial.border_strong") || "rgba(72, 77, 91, 0.95)"
        );
      }
    } catch (e) {}
  }

  function shouldLocalizeLayer(layer) {
    if (!layer || layer.type !== "symbol" || !layer.layout || !layer.layout["text-field"]) {
      return false;
    }
    var id = String(layer.id || "").toLowerCase();
    return /country|state|settlement|place|marine|water|natural|continent|ocean/i.test(id);
  }

  function localizeMapLabels(map) {
    if (!map || typeof map.getStyle !== "function" || typeof map.setLayoutProperty !== "function") {
      return;
    }
    try {
      var style = map.getStyle();
      if (!style || !Array.isArray(style.layers)) {
        return;
      }
      style.layers.forEach(function (layer) {
        if (!shouldLocalizeLayer(layer)) {
          return;
        }
        map.setLayoutProperty(layer.id, "text-field", [
          "coalesce",
          ["get", "name_pt"],
          ["get", "name"],
          ["get", "name_en"]
        ]);
      });
    } catch (e) {}
  }

  function buildViewStateSignature(map, bounds) {
    if (!map || !bounds) {
      return "";
    }
    function roundValue(value, digits) {
      var factor = Math.pow(10, digits || 0);
      return Math.round(Number(value) * factor) / factor;
    }
    var center = map.getCenter ? map.getCenter().toArray() : [0, 0];
    return JSON.stringify({
      zoom: roundValue(map.getZoom(), 3),
      lng: roundValue(center[0], 4),
      lat: roundValue(center[1], 4),
      west: roundValue(bounds.west, 4),
      south: roundValue(bounds.south, 4),
      east: roundValue(bounds.east, 4),
      north: roundValue(bounds.north, 4)
    });
  }

  function isTimelineDragTarget(target) {
    if (!target) {
      return false;
    }
    return !target.closest(
      "input, button, select, option, textarea, label, a, .irs, .irs-line, .irs-bar, .irs-slider, .irs-handle, .shiny-input-container"
    );
  }

  function readTimelineOffset(card, varName) {
    if (!card) {
      return 0;
    }
    var raw = card.style.getPropertyValue(varName) || "";
    var parsed = parseFloat(raw);
    return isFinite(parsed) ? parsed : 0;
  }

  function writeTimelineOffset(card, x, y) {
    if (!card) {
      return;
    }
    card.style.setProperty("--sp-card-dx", String(x) + "px");
    card.style.setProperty("--sp-card-dy", String(y) + "px");
  }

  function timelineBounds(shell, card) {
    var shellRect = shell.getBoundingClientRect();
    var cardRect = card.getBoundingClientRect();
    var maxX = Math.max(0, (shellRect.width - cardRect.width) / 2);
    var maxY = Math.max(0, shellRect.height - cardRect.height);
    return {
      minX: -maxX,
      maxX: maxX,
      minY: 0,
      maxY: maxY
    };
  }

  function clampTimelineCard(card) {
    if (!card) {
      return;
    }
    var shell = card.closest(".ic2025-spatial-timeline-shell");
    if (!shell) {
      return;
    }
    var limits = timelineBounds(shell, card);
    var x = clampNumber(readTimelineOffset(card, "--sp-card-dx"), limits.minX, limits.maxX);
    var y = clampNumber(readTimelineOffset(card, "--sp-card-dy"), limits.minY, limits.maxY);
    writeTimelineOffset(card, x, y);
  }

  function clampAllTimelineCards() {
    document.querySelectorAll(".ic2025-spatial-timeline-card").forEach(clampTimelineCard);
  }

  function installTimelineDrag() {
    if (window.__ic2025SpatialTimelineDragInstalled) {
      clampAllTimelineCards();
      return;
    }
    window.__ic2025SpatialTimelineDragInstalled = true;

    document.addEventListener("pointerdown", function (event) {
      if (event.button !== 0) {
        return;
      }
      var card = event.target.closest(".ic2025-spatial-timeline-card");
      if (
        !card ||
        !isTimelineDragTarget(event.target) ||
        window.innerWidth <= 768 ||
        window.getComputedStyle(card).position !== "absolute"
      ) {
        return;
      }
      var shell = card.closest(".ic2025-spatial-timeline-shell");
      if (!shell) {
        return;
      }
      var limits = timelineBounds(shell, card);
      timelineDragState = {
        card: card,
        pointerId: event.pointerId,
        startX: event.clientX,
        startY: event.clientY,
        originX: readTimelineOffset(card, "--sp-card-dx"),
        originY: readTimelineOffset(card, "--sp-card-dy"),
        limits: limits
      };
      card.classList.add("is-dragging");
      event.preventDefault();
    });

    document.addEventListener("pointermove", function (event) {
      if (!timelineDragState || event.pointerId !== timelineDragState.pointerId) {
        return;
      }
      var nextX = clampNumber(
        timelineDragState.originX + (event.clientX - timelineDragState.startX),
        timelineDragState.limits.minX,
        timelineDragState.limits.maxX
      );
      var nextY = clampNumber(
        timelineDragState.originY + (event.clientY - timelineDragState.startY),
        timelineDragState.limits.minY,
        timelineDragState.limits.maxY
      );
      writeTimelineOffset(timelineDragState.card, nextX, nextY);
    });

    function finishTimelineDrag(event) {
      if (!timelineDragState || (event && event.pointerId !== timelineDragState.pointerId)) {
        return;
      }
      if (timelineDragState.card) {
        timelineDragState.card.classList.remove("is-dragging");
        clampTimelineCard(timelineDragState.card);
      }
      timelineDragState = null;
    }

    document.addEventListener("pointerup", finishTimelineDrag);
    document.addEventListener("pointercancel", finishTimelineDrag);
    window.addEventListener("resize", clampAllTimelineCards);
    window.setTimeout(clampAllTimelineCards, 120);
  }

  function pushViewState(state) {
    if (
      !state ||
      !state.map ||
      !state.viewStateInputId ||
      !window.Shiny ||
      typeof window.Shiny.setInputValue !== "function"
    ) {
      return;
    }
    var bounds = null;
    if (state.map && typeof state.map.getBounds === "function") {
      var mapBounds = state.map.getBounds();
      if (mapBounds) {
        bounds = {
          west: mapBounds.getWest(),
          south: mapBounds.getSouth(),
          east: mapBounds.getEast(),
          north: mapBounds.getNorth()
        };
      }
    }
    var signature = buildViewStateSignature(state.map, bounds);
    if (signature && signature === state.lastViewSignature) {
      return;
    }
    state.lastViewSignature = signature;
    window.Shiny.setInputValue(
      state.viewStateInputId,
      {
        zoom: state.map.getZoom(),
        center: state.map.getCenter ? state.map.getCenter().toArray() : null,
        bounds: bounds,
        nonce: Date.now()
      },
      { priority: "event" }
    );
  }

  function ensureScaleState(state, scaleName) {
    if (!state.scaleStates[scaleName]) {
      state.scaleStates[scaleName] = {
        scale: scaleName,
        sourceId: state.id + "-source-" + scaleName,
        fillId: state.id + "-fill-" + scaleName,
        lineId: state.id + "-line-" + scaleName,
        keyProperty: scaleName === "municipio" ? "code_muni" : "abbrev_state",
        metaByKey: {},
        lastSourceSignature: null,
        lastSourceData: null,
        hoverFeatureId: null,
        handlersBound: false
      };
    }
    return state.scaleStates[scaleName];
  }

  function resolveSourceData(payload) {
    if (!payload) {
      return null;
    }
    if (payload.sourceUrl || payload.geojsonUrl) {
      return payload.sourceUrl || payload.geojsonUrl;
    }
    if (typeof payload.geojson === "string" && payload.geojson.length) {
      return JSON.parse(payload.geojson);
    }
    return null;
  }

  function themeColor(key) {
    var theme = window.IC2025_THEME_COLORS || {};
    return theme && Object.prototype.hasOwnProperty.call(theme, key) ? theme[key] : null;
  }

  function boundaryPaintForScale(scaleName) {
    if (scaleName === "state") {
      return {
        "line-color": themeColor("spatial.border_strong") || "rgba(72, 77, 91, 0.95)",
        "line-opacity": 0.96,
        "line-width": [
          "interpolate",
          ["linear"],
          ["zoom"],
          3.2, 0.9,
          4.8, 1.15,
          6.5, 1.45
        ]
      };
    }
    return {
      "line-color": themeColor("spatial.border_strong") || "rgba(72, 77, 91, 0.95)",
      "line-opacity": 0.94,
      "line-width": [
        "interpolate",
        ["linear"],
        ["zoom"],
        5.2, 1.72,
        6.4, 2.30,
        8.0, 2.84,
        10.5, 3.48
      ]
    };
  }

  function fillOutlineColorForScale(scaleName) {
    if (scaleName === "municipio") {
      return themeColor("spatial.border_strong") || "rgba(72, 77, 91, 0.95)";
    }
    return "rgba(0, 0, 0, 0)";
  }

  function boundaryLayoutForScale(scaleName, visible) {
    var layout = {
      visibility: visible ? "visible" : "none"
    };
    if (scaleName === "municipio") {
      layout["line-join"] = "round";
      layout["line-cap"] = "round";
    }
    return layout;
  }

  function findFillAnchorLayerId(map) {
    if (!map || typeof map.getStyle !== "function") {
      return null;
    }
    try {
      var style = map.getStyle();
      if (!style || !Array.isArray(style.layers)) {
        return null;
      }
      var roadLabel = style.layers.find(function (layer) {
        return layer && layer.id === "road-label-simple";
      });
      if (roadLabel && roadLabel.id) {
        return roadLabel.id;
      }
      var preferredSymbol = style.layers.find(function (layer) {
        return layer &&
          layer.type === "symbol" &&
          layer.layout &&
          layer.layout["text-field"] &&
          /label|place|settlement|road|water|natural/i.test(String(layer.id || ""));
      });
      if (preferredSymbol && preferredSymbol.id) {
        return preferredSymbol.id;
      }
      var pitchOutline = style.layers.find(function (layer) {
        return layer && layer.id === "pitch-outline";
      });
      if (pitchOutline && pitchOutline.id) {
        return pitchOutline.id;
      }
      var building = style.layers.find(function (layer) {
        return layer && layer.id === "building";
      });
      if (building && building.id) {
        return building.id;
      }
      var fallback = style.layers.find(function (layer) {
        return layer && layer.type === "symbol" && layer.id;
      });
      return fallback ? fallback.id : null;
    } catch (e) {
      return null;
    }
  }

  function setLayerVisibility(map, layerId, visible) {
    if (!map.getLayer(layerId)) {
      return;
    }
    map.setLayoutProperty(layerId, "visibility", visible ? "visible" : "none");
  }

  function bindScaleHandlers(state, scaleName) {
    var scaleState = ensureScaleState(state, scaleName);
    if (!state.map || scaleState.handlersBound || !state.map.getLayer(scaleState.fillId)) {
      return;
    }

    scaleState.handlersBound = true;

    state.map.on("mouseenter", scaleState.fillId, function () {
      state.map.getCanvas().style.cursor = "pointer";
    });

    state.map.on("mousemove", scaleState.fillId, function (event) {
      var feature = event.features && event.features[0];
      if (!feature) {
        return;
      }
      var featureId = feature.id;
      var hoverKey = feature.properties && feature.properties[scaleState.keyProperty] != null
        ? String(feature.properties[scaleState.keyProperty])
        : "";
      var meta = scaleState.metaByKey && scaleState.metaByKey[hoverKey] ? scaleState.metaByKey[hoverKey] : null;
      if (scaleState.hoverFeatureId != null && scaleState.hoverFeatureId !== featureId) {
        state.map.setFeatureState(
          { source: scaleState.sourceId, id: scaleState.hoverFeatureId },
          { hover: false }
        );
      }
      if (featureId != null && scaleState.hoverFeatureId !== featureId) {
        state.map.setFeatureState(
          { source: scaleState.sourceId, id: featureId },
          { hover: true }
        );
        scaleState.hoverFeatureId = featureId;
      }
      if (state.popup) {
        state.popup
          .setLngLat(event.lngLat)
          .setHTML(popupHtml(meta, state.metricLabel))
          .addTo(state.map);
      }
    });

    state.map.on("mouseleave", scaleState.fillId, function () {
      state.map.getCanvas().style.cursor = "";
      if (scaleState.hoverFeatureId != null) {
        state.map.setFeatureState(
          { source: scaleState.sourceId, id: scaleState.hoverFeatureId },
          { hover: false }
        );
        scaleState.hoverFeatureId = null;
      }
      if (state.popup) {
        state.popup.remove();
      }
    });

    state.map.on("click", scaleState.fillId, function (event) {
      var feature = event.features && event.features[0];
      var payload = state.lastPayload || {};
      var clickKey = feature && feature.properties && feature.properties[scaleState.keyProperty] != null
        ? String(feature.properties[scaleState.keyProperty])
        : null;
      var meta = clickKey && scaleState.metaByKey ? scaleState.metaByKey[clickKey] : null;
      if (!feature || !window.Shiny || typeof window.Shiny.setInputValue !== "function") {
        return;
      }
      window.Shiny.setInputValue(
        payload.clickInputId,
        {
          id: clickKey,
          label: meta ? meta.placeLabel : null,
          scale: scaleName,
          nonce: Date.now()
        },
        { priority: "event" }
      );
    });
  }

  function preloadScaleSource(state, scaleName, sourceUrl) {
    if (!state.map || !sourceUrl) {
      return;
    }
    var scaleState = ensureScaleState(state, scaleName);
    if (!state.map.getSource(scaleState.sourceId)) {
      state.map.addSource(scaleState.sourceId, {
        type: "geojson",
        data: sourceUrl,
        generateId: true
      });
      scaleState.lastSourceData = sourceUrl;
    }
  }

  function fallbackScalePayload(payload) {
    var scaleName = payload.scale || payload.activeScale || "state";
    var out = {};
    out[scaleName] = {
      sourceUrl: payload.sourceUrl,
      geojsonUrl: payload.geojsonUrl,
      geojson: payload.geojson,
      sourceKeyProperty: payload.sourceKeyProperty,
      visibleKeys: payload.visibleKeys,
      featureData: payload.featureData || []
    };
    return out;
  }

  function resetManualDrill(state) {
    if (!state || !state.manualDrill) {
      return;
    }
    state.manualDrill.pendingAnchor = false;
    state.manualDrill.anchorZoom = null;
    state.manualDrill.lastZoom = null;
    state.manualDrill.zoomOutSteps = 0;
  }

  function armManualDrill(state) {
    if (!state || !state.manualDrill) {
      return;
    }
    state.manualDrill.pendingAnchor = true;
    state.manualDrill.anchorZoom = null;
    state.manualDrill.lastZoom = null;
    state.manualDrill.zoomOutSteps = 0;
  }

  function notifyManualDrillReset(state) {
    if (
      !state ||
      !state.drillResetInputId ||
      !window.Shiny ||
      typeof window.Shiny.setInputValue !== "function"
    ) {
      return;
    }
    window.Shiny.setInputValue(
      state.drillResetInputId,
      { nonce: Date.now() },
      { priority: "event" }
    );
  }

  function forceVisibleScale(state, nextScale) {
    if (!state || !state.map || !state.payloadByScale || !state.payloadByScale[nextScale]) {
      return false;
    }
    applyScalePayload(state, nextScale, state.payloadByScale[nextScale], nextScale);
    Object.keys(state.scaleStates).forEach(function (existingScale) {
      var existing = ensureScaleState(state, existingScale);
      var isVisible = existingScale === nextScale;
      setLayerVisibility(state.map, existing.fillId, isVisible);
      setLayerVisibility(state.map, existing.lineId, isVisible);
    });
    state.visibleScale = nextScale;
    state.clientScaleOverride = nextScale;
    return true;
  }

  function resolveVisibleScale(state, payload) {
    var payloads = payload.scalePayloads || fallbackScalePayload(payload);
    var requestedScale = payload.activeScale || payload.scale || state.visibleScale || "state";
    var autoScaleEnabled = payload.autoScaleEnabled === true;
    if (
      autoScaleEnabled !== true &&
      state.clientScaleOverride &&
      payloads[state.clientScaleOverride]
    ) {
      return state.clientScaleOverride;
    }
    if (
      autoScaleEnabled &&
      state.map &&
      payloads.state &&
      payloads.municipio
    ) {
      var zoom = Number(state.map.getZoom());
      if (isFinite(zoom)) {
        var currentVisible = state.visibleScale || requestedScale;
        var zoomIn = Number(payload.zoomThresholdIn || 6.8);
        var zoomOut = Number(payload.zoomThresholdOut || 6.4);
        if (currentVisible === "municipio") {
          return zoom <= zoomOut ? "state" : "municipio";
        }
        return zoom >= zoomIn ? "municipio" : "state";
      }
    }
    if (payloads[requestedScale]) {
      return requestedScale;
    }
    if (state.visibleScale && payloads[state.visibleScale]) {
      return state.visibleScale;
    }
    return payloads.state ? "state" : (Object.keys(payloads)[0] || requestedScale);
  }

  function updateVisibleScale(state, payload) {
    if (!state || !state.map || !payload) {
      return;
    }
    var payloads = payload.scalePayloads || fallbackScalePayload(payload);
    var nextScale = resolveVisibleScale(state, payload);
    if (payloads[nextScale]) {
      applyScalePayload(state, nextScale, payloads[nextScale], nextScale);
    }
    Object.keys(state.scaleStates).forEach(function (existingScale) {
      var existing = ensureScaleState(state, existingScale);
      var isVisible = existingScale === nextScale;
      setLayerVisibility(state.map, existing.fillId, isVisible);
      setLayerVisibility(state.map, existing.lineId, isVisible);
    });
    state.visibleScale = nextScale;
  }

  function applyScalePayload(state, scaleName, payload, visibleScale) {
    var scaleState = ensureScaleState(state, scaleName);
    var fillAnchorId = state.fillAnchorId || findFillAnchorLayerId(state.map);
    var sourceSignature = payload.sourceSignature || payload.sourceUrl || payload.geojsonUrl || payload.geojson || "";
    var data = null;
    scaleState.keyProperty = payload.sourceKeyProperty || scaleState.keyProperty;
    scaleState.metaByKey = {};
    (payload.featureData || []).forEach(function (item) {
      if (!item || item.key == null) {
        return;
      }
      scaleState.metaByKey[String(item.key)] = item;
    });

    var fillFilter = buildVisibleFilter(scaleState.keyProperty, payload.visibleKeys);
    var fillColor = buildMatchExpression(
      payload.featureData,
      scaleState.keyProperty,
      "fillColor",
      themeColor("spatial.legend_fallback") || "#D7DCE6"
    );
    var fillOpacity = buildMatchExpression(
      payload.featureData,
      scaleState.keyProperty,
      "fillOpacity",
      0.45
    );
    var fillOutlineColor = fillOutlineColorForScale(scaleName);
    var shouldDrawBoundary = scaleName === "municipio" || scaleName === "state";
    var boundaryPaint = boundaryPaintForScale(scaleName);
    var boundaryLayout = boundaryLayoutForScale(scaleName, visibleScale === scaleName);

    if (!state.map.getSource(scaleState.sourceId)) {
      data = scaleState.lastSourceSignature === sourceSignature && scaleState.lastSourceData
        ? scaleState.lastSourceData
        : resolveSourceData(payload);
      if (!data) {
        return;
      }
      state.map.addSource(scaleState.sourceId, {
        type: "geojson",
        data: data,
        generateId: true
      });
      scaleState.lastSourceSignature = sourceSignature;
      scaleState.lastSourceData = data;
    } else {
      var sourceChanged = sourceSignature !== scaleState.lastSourceSignature;
      if (sourceChanged) {
        data = resolveSourceData(payload);
        if (!data) {
          return;
        }
        state.map.getSource(scaleState.sourceId).setData(data);
        scaleState.lastSourceSignature = sourceSignature;
        scaleState.lastSourceData = data;
        scaleState.hoverFeatureId = null;
      }
    }

    if (!state.map.getLayer(scaleState.fillId)) {
      var fillPaint = {
        "fill-color": fillColor,
        "fill-opacity": fillOpacity,
        "fill-outline-color": fillOutlineColor
      };
      state.map.addLayer(
        {
          id: scaleState.fillId,
          type: "fill",
          source: scaleState.sourceId,
          filter: fillFilter,
          layout: {
            visibility: visibleScale === scaleName ? "visible" : "none"
          },
          paint: fillPaint
        },
        fillAnchorId || undefined
      );
    } else {
      state.map.setFilter(scaleState.fillId, fillFilter);
      state.map.setPaintProperty(scaleState.fillId, "fill-color", fillColor);
      state.map.setPaintProperty(scaleState.fillId, "fill-opacity", fillOpacity);
      state.map.setPaintProperty(scaleState.fillId, "fill-outline-color", fillOutlineColor);
      setLayerVisibility(state.map, scaleState.fillId, visibleScale === scaleName);
    }

    if (shouldDrawBoundary) {
      if (!state.map.getLayer(scaleState.lineId)) {
        state.map.addLayer(
          {
            id: scaleState.lineId,
            type: "line",
            source: scaleState.sourceId,
            filter: fillFilter,
            layout: boundaryLayout,
            paint: boundaryPaint
          },
          fillAnchorId || undefined
        );
      } else {
        state.map.setFilter(scaleState.lineId, fillFilter);
        state.map.setPaintProperty(scaleState.lineId, "line-color", boundaryPaint["line-color"]);
        state.map.setPaintProperty(scaleState.lineId, "line-opacity", boundaryPaint["line-opacity"]);
        state.map.setPaintProperty(scaleState.lineId, "line-width", boundaryPaint["line-width"]);
        if (scaleName === "municipio") {
          state.map.setLayoutProperty(scaleState.lineId, "line-join", "round");
          state.map.setLayoutProperty(scaleState.lineId, "line-cap", "round");
        }
        setLayerVisibility(state.map, scaleState.lineId, visibleScale === scaleName);
      }
    }

    bindScaleHandlers(state, scaleName);
  }

  function applyMapPayload(state, payload) {
    if (!state || !state.map || !payload || !state.loaded) {
      return;
    }

    state.metricLabel = payload.metricLabel || "";
    if (payload.autoScaleEnabled === true) {
      state.clientScaleOverride = null;
    } else if (
      state.clientScaleOverride &&
      (payload.activeScale === state.clientScaleOverride || payload.scale === state.clientScaleOverride)
    ) {
      state.clientScaleOverride = null;
    }
    if (payload.resetScalePayloads) {
      state.payloadByScale = {};
    }
    var scalePayloads = payload.scalePayloads || fallbackScalePayload(payload);
    Object.keys(scalePayloads).forEach(function (scaleName) {
      state.payloadByScale[scaleName] = scalePayloads[scaleName];
    });
    state.lastPayload = Object.assign({}, state.lastPayload || {}, payload, {
      scalePayloads: state.payloadByScale
    });
    var activeScale = payload.activeScale || payload.scale || "state";

    if (scalePayloads[activeScale]) {
      applyScalePayload(state, activeScale, scalePayloads[activeScale], activeScale);
    }
    Object.keys(scalePayloads).forEach(function (scaleName) {
      if (scaleName === activeScale) {
        return;
      }
      applyScalePayload(state, scaleName, scalePayloads[scaleName], activeScale);
    });
    updateVisibleScale(state, state.lastPayload);
    if (payload.autoScaleEnabled === false) {
      if (state.visibleScale === "state") {
        resetManualDrill(state);
      } else if (
        state.visibleScale === "municipio" &&
        !state.manualDrill.anchorZoom &&
        !state.manualDrill.pendingAnchor
      ) {
        armManualDrill(state);
      }
    }

    if (payload.fitBounds) {
      var bounds = buildBounds(payload.bbox);
      if (bounds) {
        var fitPadding = payload.fitPadding || { top: 36, right: 36, bottom: 36, left: 36 };
        state.map.fitBounds(bounds, {
          padding: fitPadding,
          duration: 700,
          maxZoom: Number(payload.fitMaxZoom || 9.6)
        });
      }
    }

    if (payload.timelineValueOutputId && typeof payload.timelineValueText === "string") {
      var timelineNode = document.getElementById(payload.timelineValueOutputId);
      if (timelineNode) {
        timelineNode.textContent = payload.timelineValueText;
      }
    }

    if (
      payload.frameAppliedInputId &&
      window.Shiny &&
      typeof window.Shiny.setInputValue === "function"
    ) {
      var notifyFrameApplied = function () {
        window.Shiny.setInputValue(
          payload.frameAppliedInputId,
          {
            key: payload.frameKey || "",
            displayValue: payload.timelineValueText || "",
            nonce: Date.now()
          },
          { priority: "event" }
        );
      };
      if (state.map && typeof state.map.once === "function") {
        state.map.once("idle", notifyFrameApplied);
      } else {
        window.requestAnimationFrame(notifyFrameApplied);
      }
    }
  }

  function ensureMapState(payload) {
    var state = mapRegistry[payload.id];
    if (state) {
      state.lastPayload = payload;
      state.viewStateInputId = payload.viewStateInputId || state.viewStateInputId || null;
      return state;
    }

    state = {
      id: payload.id,
      popup: null,
      loaded: false,
      bootReady: false,
      lastPayload: payload,
      lastViewSignature: "",
      payloadByScale: {},
      scaleStates: {},
      metricLabel: "",
      visibleScale: payload.activeScale || payload.scale || "state",
      viewStateInputId: payload.viewStateInputId || null,
      drillResetInputId: payload.drillResetInputId || null,
      clientScaleOverride: null,
      fillAnchorId: null,
      map: null,
      root: null,
      manualDrill: {
        pendingAnchor: false,
        anchorZoom: null,
        lastZoom: null,
        zoomOutSteps: 0
      }
    };
    mapRegistry[payload.id] = state;

    var container = document.getElementById(payload.id);
    if (!container) {
      return state;
    }
    state.root = container.closest(".ic2025-spatial-app");

    window.mapboxgl.accessToken = payload.accessToken;
    state.map = new window.mapboxgl.Map({
      container: payload.id,
      style: payload.styleUrl,
      center: [-52.5, -14.5],
      zoom: 3.2,
      attributionControl: true,
      pitchWithRotate: false
    });
    state.map.dragRotate.disable();
    state.map.touchZoomRotate.disableRotation();
    state.popup = new window.mapboxgl.Popup({
      closeButton: false,
      closeOnClick: false,
      maxWidth: "280px",
      className: "ic2025-spatial-mapbox-popup"
    });

    state.map.on("load", function () {
      state.loaded = true;
      localizeMapLabels(state.map);
      state.fillAnchorId = findFillAnchorLayerId(state.map);
      strengthenBaseBoundaries(state.map);
      applyMapPayload(state, state.lastPayload);
      state.map.once("idle", function () {
        markSpatialBootReady(state);
      });
      pushViewState(state);
    });

    state.map.on("style.load", function () {
      state.loaded = true;
      Object.keys(state.scaleStates).forEach(function (scaleName) {
        var scaleState = ensureScaleState(state, scaleName);
        scaleState.handlersBound = false;
        scaleState.hoverFeatureId = null;
      });
      localizeMapLabels(state.map);
      state.fillAnchorId = findFillAnchorLayerId(state.map);
      strengthenBaseBoundaries(state.map);
      applyMapPayload(state, state.lastPayload);
      state.map.once("idle", function () {
        markSpatialBootReady(state);
      });
      pushViewState(state);
    });

    state.map.on("moveend", function () {
      pushViewState(state);
    });

    state.map.on("zoomend", function () {
      if (state.lastPayload && state.lastPayload.autoScaleEnabled === false) {
        var zoom = Number(state.map.getZoom());
        if (isFinite(zoom)) {
          if (state.manualDrill.pendingAnchor) {
            state.manualDrill.anchorZoom = zoom;
            state.manualDrill.lastZoom = zoom;
            state.manualDrill.zoomOutSteps = 0;
            state.manualDrill.pendingAnchor = false;
          } else if (state.manualDrill.anchorZoom != null && state.manualDrill.lastZoom != null) {
            if (zoom < state.manualDrill.lastZoom - 0.015) {
              state.manualDrill.zoomOutSteps += 1;
            }
            state.manualDrill.lastZoom = zoom;
            if (state.manualDrill.zoomOutSteps >= 3) {
              forceVisibleScale(state, "state");
              notifyManualDrillReset(state);
              resetManualDrill(state);
            }
          }
        }
      }
      if (state.lastPayload) {
        updateVisibleScale(state, state.lastPayload);
      }
      pushViewState(state);
    });

    return state;
  }

  function registerHandlers() {
    if (!window.Shiny || typeof window.Shiny.addCustomMessageHandler !== "function") {
      window.setTimeout(registerHandlers, 80);
      return;
    }

    if (window.__ic2025SpatialHandlersRegistered) {
      return;
    }
    window.__ic2025SpatialHandlersRegistered = true;

    window.Shiny.addCustomMessageHandler("ic2025-spatial-shell", function (message) {
      setSpatialShell(message && message.active);
    });

    window.Shiny.addCustomMessageHandler("ic2025-spatial-resize", function (message) {
      setTimeout(triggerResize, 60);
      setTimeout(triggerResize, 220);
    });

    window.Shiny.addCustomMessageHandler("ic2025-spatial-mapbox", function (payload) {
      ensureMapboxReady(function () {
        var state = ensureMapState(payload);
        applyMapPayload(state, payload);
      });
    });

    window.Shiny.addCustomMessageHandler("ic2025-spatial-view-state", function (message) {
      if (!message || !message.id) {
        return;
      }
      var root = document.getElementById(message.id);
      if (!root) {
        return;
      }
      root.setAttribute("data-view-tab", String(message.view || "map"));
      clampAllTimelineCards();
    });

    window.Shiny.addCustomMessageHandler("ic2025-spatial-prefs-load", function (message) {
      if (!message || !message.inputId || !window.Shiny || typeof window.Shiny.setInputValue !== "function") {
        return;
      }
      var prefs = readStorageJson(message.storageKey);
      if (!prefs) {
        return;
      }
      window.Shiny.setInputValue(
        message.inputId,
        {
          prefs: prefs,
          nonce: Date.now()
        },
        { priority: "event" }
      );
    });

    window.Shiny.addCustomMessageHandler("ic2025-spatial-prefs-save", function (message) {
      if (!message || !message.storageKey) {
        return;
      }
      writeStorageJson(message.storageKey, message.preferences || {});
    });

    window.Shiny.addCustomMessageHandler("ic2025-spatial-prefs-clear", function (message) {
      if (!message || !message.storageKey) {
        return;
      }
      removeStorageKey(message.storageKey);
    });

    window.Shiny.addCustomMessageHandler("ic2025-spatial-tutorial-autostart", function (message) {
      if (
        !message ||
        !message.inputId ||
        !window.Shiny ||
        typeof window.Shiny.setInputValue !== "function" ||
        window.innerWidth <= 768
      ) {
        return;
      }
      var lastSeen = null;
      if (storageAvailable() && message.storageKey) {
        lastSeen = Number(window.localStorage.getItem(message.storageKey) || 0);
      }
      var now = Date.now();
      var cooldown = Number(message.cooldownDays || 180) * 24 * 60 * 60 * 1000;
      if (!lastSeen || !isFinite(lastSeen) || (now - lastSeen) >= cooldown) {
        window.Shiny.setInputValue(message.inputId, now, { priority: "event" });
      }
    });

    window.Shiny.addCustomMessageHandler("ic2025-spatial-tutorial-mark-seen", function (message) {
      if (!message || !message.storageKey || !storageAvailable()) {
        return;
      }
      try {
        window.localStorage.setItem(message.storageKey, String(Date.now()));
      } catch (e) {}
    });

    window.Shiny.addCustomMessageHandler("ic2025-spatial-return", function (message) {
      var target = (message && message.target) || "desc";
      var link = document.querySelector('.sidebar-menu a[data-value="' + target + '"]');
      if (link) {
        link.click();
      }
    });

    window.Shiny.addCustomMessageHandler("ic2025-spatial-introjs-cleanup", function () {
      cleanupIntroJsArtifacts();
    });
  }

  registerHandlers();
  installSidebarObserver();
  installShellPoller();
  installSpatialGroupTabs();
  installTimelineDrag();
  document.addEventListener("shiny:connected", registerHandlers);
  document.addEventListener("shiny:connected", installSidebarObserver);
  document.addEventListener("shiny:connected", installShellPoller);
  document.addEventListener("shiny:connected", installSpatialGroupTabs);
  document.addEventListener("shiny:connected", installTimelineDrag);

  document.addEventListener("shown.bs.tab", function () {
    setTimeout(syncShellFromSidebar, 20);
    setTimeout(function () {
      syncSpatialGroupTabs(document);
    }, 40);
    setTimeout(triggerResize, 80);
  });

  document.addEventListener("click", function (event) {
    if (event.target.closest("#spatial_app-back")) {
      window.setTimeout(function () {
        setSpatialShell(false);
      }, 20);
    }

    var link = event.target.closest('.sidebar-menu a[data-value]');
    if (link) {
      if (link.getAttribute("data-value") !== "spatial") {
        window.setTimeout(function () {
          setSpatialShell(false);
        }, 20);
      }
      setTimeout(syncShellFromSidebar, 40);
    }
  });

  window.addEventListener("orientationchange", function () {
    setTimeout(triggerResize, 140);
  });

  window.addEventListener("load", function () {
    installSidebarObserver();
    installShellPoller();
    installSpatialGroupTabs();
    installTimelineDrag();
    setTimeout(syncShellFromSidebar, 140);
  });
})();
