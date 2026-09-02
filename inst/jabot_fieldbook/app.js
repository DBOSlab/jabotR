(() => {
  "use strict";

  const STORAGE_KEY = "jabotR_specimen_form_draft_v6";

  const TOOLTIP_TEXT = {
    numtombo: "Ao informar o número de tombo, o sistema mantém o tombo informado.",
    sufixo: "Sufixo do número de tombo. Ex.: a, b, c.",
    family: "Família botânica do espécime.",
    genus: "Gênero botânico do espécime.",
    cf: "Detalhe da identificação taxonômica. Preencher com cf. ou aff., quando necessário.",
    sp1: "Espécie.",
    author1: "Autor da espécie.",
    rank1: "Tipo da primeira infraespécie: var., f., subsp., ssp. ou infr.",
    sp2: "Epíteto da primeira infraespécie.",
    author2: "Autor da primeira infraespécie.",
    rank2: "Tipo da segunda infraespécie.",
    sp3: "Epíteto da segunda infraespécie.",
    author3: "Autor da segunda infraespécie.",
    vernacular: "Nome vulgar pelo qual a espécie é conhecida na região.",
    typestat: "Status de tipo do material, quando aplicável.",
    country: "País onde a coleta foi realizada.",
    majorarea: "Estado onde a coleta foi realizada.",
    minorarea: "Município onde a coleta foi realizada.",
    gazetteer: "Localidade dentro do município, como fazenda, rio, estrada ou comunidade.",
    uc: "Unidade de conservação, Terra Indígena ou outra área protegida.",
    latitude: "Latitude em formato decimal.",
    longitude: "Longitude em formato decimal.",
    lat_grau: "Graus de latitude.",
    lat_min: "Minutos de latitude.",
    lat_seg: "Segundos de latitude.",
    ns: "Hemisfério da latitude: N ou S.",
    long_grau: "Graus de longitude.",
    long_min: "Minutos de longitude.",
    long_seg: "Segundos de longitude.",
    ew: "Hemisfério da longitude: E ou W.",
    altprof: "Altitude ou profundidade mínima, sem a unidade.",
    altprofmax: "Altitude ou profundidade máxima, sem a unidade.",
    unidmedaltprof: "Unidade da altitude ou profundidade, por exemplo m, cm, mm ou ft.",
    locnotes: "Observações detalhadas sobre a localidade da coleta.",
    flor: "Informação sobre presença de flores.",
    fruto: "Informação sobre presença de frutos.",
    fuste: "Descrição ou valor relacionado ao fuste.",
    altura: "Altura do indivíduo, sem a unidade de medida.",
    unidmedaltura: "Unidade da altura, por exemplo m, cm, mm ou ft.",
    collector: "Coletor principal.",
    number: "Número da coleta do coletor principal.",
    addcoll: "Coletores adicionais.",
    colldd: "Dia da coleta.",
    collmm: "Mês da coleta em algarismos.",
    collyy: "Ano da coleta com quatro algarismos.",
    detby: "Determinador ou determinadores da identificação.",
    detdd: "Dia da determinação.",
    detmm: "Mês da determinação.",
    detyy: "Ano da determinação com quatro algarismos.",
    sigla_colbot_origem: "Sigla da coleção botânica de origem.",
    dups: "Siglas dos herbários que receberão duplicatas.",
    nrdups: "Quantidade de duplicatas ou etiquetas associadas.",
    notes: "Descrição do indivíduo. Altura e habitat possuem campos próprios.",
    usos: "Informações etnobotânicas ou categorias de uso.",
    uso_especifico: "Descrição específica dos usos da planta.",
    projeto: "Projeto associado à coleta.",
    habitat: "Descrição do habitat do espécime.",
    habito: "Hábito do indivíduo. Ex.: árvore, arbusto, erva, liana ou palmeira."
  };

  const JABOT_COLUMNS = [
    "numtombo", "sufixo", "family", "genus", "cf", "sp1", "author1", "rank1",
    "sp2", "author2", "rank2", "sp3", "author3", "vernacular", "typestat",
    "country", "majorarea", "minorarea", "gazetteer", "uc", "latitude",
    "longitude", "lat_grau", "lat_min", "lat_seg", "ns", "long_grau",
    "long_min", "long_seg", "ew", "altprof", "altprofmax", "unidmedaltprof",
    "locnotes", "flor", "fruto", "fuste", "altura", "unidmedaltura",
    "collector", "number", "addcoll", "colldd", "collmm", "collyy", "detby",
    "detdd", "detmm", "detyy", "sigla_colbot_origem", "dups", "nrdups",
    "notes", "usos", "uso_especifico", "projeto", "habitat", "habito"
  ];

  const DAY_MAIN_FIELDS = [
    { name: "colldd", label: "Dia", span: 2, numeric: true },
    { name: "collmm", label: "Mês", span: 2, numeric: true },
    { name: "collyy", label: "Ano", span: 2, numeric: true },
    { name: "country", label: "País", span: 2 },
    { name: "majorarea", label: "Estado", span: 2 },
    { name: "minorarea", label: "Município", span: 2 },
    { name: "gazetteer", label: "Localidade", span: 4 },
    { name: "uc", label: "UC / TI / área protegida", span: 4 },
    { name: "habitat", label: "Habitat", span: 4 },
    { name: "latitude", label: "Latitude", span: 2 },
    { name: "longitude", label: "Longitude", span: 2 },
    { name: "altprof", label: "Altitude", span: 2, numeric: true },
    { name: "collector", label: "Collector", span: 3 },
    { name: "addcoll", label: "Addcoll", span: 3 },
    { name: "locnotes", label: "Locnotes", span: 12, textarea: true }
  ];

  const DAY_EXTRA_FIELDS = [
    { name: "lat_grau", label: "Lat. grau", span: 2, numeric: true },
    { name: "lat_min", label: "Lat. min", span: 2, numeric: true },
    { name: "lat_seg", label: "Lat. seg", span: 2, numeric: true },
    { name: "ns", label: "N/S", span: 2, select: ["", "N", "S"] },
    { name: "long_grau", label: "Long. grau", span: 2, numeric: true },
    { name: "long_min", label: "Long. min", span: 2, numeric: true },
    { name: "long_seg", label: "Long. seg", span: 2, numeric: true },
    { name: "ew", label: "E/W", span: 2, select: ["", "E", "W"] },
    { name: "altprofmax", label: "Altitude máxima", span: 2, numeric: true },
    { name: "unidmedaltprof", label: "Unid. altitude", span: 2 },
    { name: "sigla_colbot_origem", label: "Herbário origem", span: 2 },
    { name: "dups", label: "Dups", span: 2 },
    { name: "nrdups", label: "Nº dups", span: 2, numeric: true },
    { name: "projeto", label: "Projeto", span: 6 }
  ];

  const SPECIMEN_FIELDS = [
    { name: "number", label: "Nº", width: 118, min: 108, max: 190, numeric: true },
    { name: "family", label: "Fam", width: 210, min: 160, max: 420 },
    { name: "genus", label: "Gen", width: 180, min: 140, max: 340 },
    { name: "cf", label: "cf.", width: 96, min: 76, max: 160 },
    { name: "sp1", label: "Sp", width: 200, min: 150, max: 420 },
    { name: "detby", label: "Det by", width: 180, min: 140, max: 360 },
    { name: "detdd", label: "Det dd", width: 112, min: 98, max: 170, numeric: true },
    { name: "detmm", label: "Det mm", width: 112, min: 98, max: 170, numeric: true },
    { name: "detyy", label: "Det yy", width: 130, min: 110, max: 190, numeric: true },
    { name: "notes", label: "Plant description", width: 280, min: 220, max: 620, textarea: true }
  ];

  const SPECIMEN_EXTRA_FIELDS = [
    { name: "numtombo", label: "numtombo", span: 2 },
    { name: "sufixo", label: "sufixo", span: 2 },
    { name: "author1", label: "author1", span: 3 },
    { name: "rank1", label: "rank1", span: 2 },
    { name: "sp2", label: "sp2", span: 3 },
    { name: "author2", label: "author2", span: 3 },
    { name: "rank2", label: "rank2", span: 2 },
    { name: "sp3", label: "sp3", span: 3 },
    { name: "author3", label: "author3", span: 3 },
    { name: "vernacular", label: "vernacular", span: 3 },
    { name: "typestat", label: "typestat", span: 3 },
    { name: "flor", label: "flor", span: 3 },
    { name: "fruto", label: "fruto", span: 3 },
    { name: "fuste", label: "fuste", span: 3 },
    { name: "altura", label: "altura", span: 2, numeric: true },
    { name: "unidmedaltura", label: "unidmedaltura", span: 2 },
    { name: "habito", label: "habito", span: 3 },
    { name: "usos", label: "usos", span: 4 },
    { name: "uso_especifico", label: "uso_especifico", span: 4 }
  ];

  const DETAIL_GROUPS = [
    {
      title: "Identificação taxonômica",
      fields: [
        "family", "genus", "cf", "sp1", "author1", "rank1", "sp2",
        "author2", "rank2", "sp3", "author3", "vernacular", "typestat"
      ]
    },
    {
      title: "Coleta e determinação",
      fields: ["number", "detby", "detdd", "detmm", "detyy", "numtombo", "sufixo"]
    },
    {
      title: "Descrição do indivíduo",
      fields: ["flor", "fruto", "fuste", "altura", "unidmedaltura", "habito", "notes"]
    },
    {
      title: "Uso e observações complementares",
      fields: ["usos", "uso_especifico"]
    }
  ];

  const ALL_SPECIMEN_FIELDS = [...SPECIMEN_FIELDS, ...SPECIMEN_EXTRA_FIELDS];

  const FIELD_BY_NAME = Object.fromEntries(
    [...DAY_MAIN_FIELDS, ...DAY_EXTRA_FIELDS, ...ALL_SPECIMEN_FIELDS]
      .map(field => [field.name, field])
  );

  const COPY_HEADER_FIELDS = [
    "country", "majorarea", "minorarea", "gazetteer", "uc", "latitude",
    "longitude", "lat_grau", "lat_min", "lat_seg", "ns", "long_grau",
    "long_min", "long_seg", "ew", "altprof", "altprofmax", "unidmedaltprof",
    "locnotes", "collector", "addcoll", "sigla_colbot_origem", "dups",
    "nrdups", "projeto", "habitat"
  ];

  const NUMERIC_COLUMNS = new Set([
    "numtombo", "number", "colldd", "collmm", "collyy", "detdd", "detmm",
    "detyy", "latitude", "longitude", "lat_grau", "lat_min", "lat_seg",
    "long_grau", "long_min", "long_seg", "altprof", "altprofmax", "nrdups",
    "altura"
  ]);

  const defaultColumnWidths = Object.fromEntries(
    SPECIMEN_FIELDS.map(field => [field.name, field.width])
  );

  const state = {
    events: [createEvent({ country: "Brasil" })],
    columnWidths: { ...defaultColumnWidths }
  };

  let pendingRemoveEventId = null;
  let pendingRemoveTimer = null;
  let pendingExportWithWarnings = false;

  const eventList = document.getElementById("event-list");
  const validationPanel = document.getElementById("validation-panel");
  const toastEl = document.getElementById("jb-toast");
  const tooltipEl = document.getElementById("jb-tooltip");

  if (!eventList || !validationPanel || !toastEl || !tooltipEl) {
    console.error(
      "jabotR Field Book: elementos obrigatórios não foram encontrados no index.html."
    );
    return;
  }

  function uid(prefix) {
    return `${prefix}_${Math.random().toString(36).slice(2, 10)}`;
  }

  function t(key, vars = {}, fallback = "") {
    if (window.JabotI18n && typeof window.JabotI18n.t === "function") {
      return window.JabotI18n.t(key, vars, fallback);
    }

    let result = fallback;
    Object.entries(vars || {}).forEach(([name, value]) => {
      result = result.replace(
        new RegExp(`\\{${name}\\}`, "g"),
        String(value)
      );
    });
    return result;
  }

  function escapeHtml(value) {
    return String(value ?? "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#039;");
  }

  function isFilled(value) {
    return String(value ?? "").trim() !== "";
  }

  function tooltip(name, fallback = "Campo da planilha padrão Jabot.") {
    return TOOLTIP_TEXT[name] || fallback;
  }

  function helpIcon(name, fallback) {
    const text = tooltip(name, fallback);
    return `<span class="jb-help" tabindex="0" aria-label="${escapeHtml(text)}" data-tip="${escapeHtml(text)}">?</span>`;
  }

  function createHeader(seed = {}) {
    const header = {};

    [...DAY_MAIN_FIELDS, ...DAY_EXTRA_FIELDS].forEach(field => {
      header[field.name] = seed[field.name] ?? "";
    });

    if (!header.country) {
      header.country = "Brasil";
    }

    return header;
  }

  function createSpecimen(seed = {}) {
    const specimen = {
      id: seed.id || uid("sp"),
      done: Boolean(seed.done),
      detailsOpen: Boolean(seed.detailsOpen)
    };

    ALL_SPECIMEN_FIELDS.forEach(field => {
      specimen[field.name] = seed[field.name] ?? "";
    });

    return specimen;
  }

  function createEvent(seed = {}) {
    return {
      id: seed.id || uid("event"),
      collapsed: Boolean(seed.collapsed),
      header: createHeader(seed.header || seed),
      specimens:
        Array.isArray(seed.specimens) && seed.specimens.length
          ? seed.specimens.map(createSpecimen)
          : [createSpecimen()]
    };
  }

  function eventDate(eventItem) {
    const d = eventItem.header.colldd;
    const m = eventItem.header.collmm;
    const y = eventItem.header.collyy;

    return [d, m, y].filter(isFilled).join("/") || "data não preenchida";
  }

  function eventPlace(eventItem) {
    return (
      eventItem.header.gazetteer ||
      eventItem.header.minorarea ||
      eventItem.header.majorarea ||
      "localidade não preenchida"
    );
  }

  function headerProgress(eventItem) {
    const essential = [
      "colldd", "collmm", "collyy", "country",
      "majorarea", "minorarea", "gazetteer", "collector"
    ];

    const filled = essential.filter(
      name => isFilled(eventItem.header[name])
    ).length;

    return Math.round((filled / essential.length) * 100);
  }

  function findEvent(eventId) {
    return state.events.find(eventItem => eventItem.id === eventId);
  }

  function findSpecimen(eventItem, specimenId) {
    return eventItem?.specimens.find(specimen => specimen.id === specimenId);
  }

  function isSpecimenEmpty(specimen) {
    return ALL_SPECIMEN_FIELDS.every(
      field => !isFilled(specimen[field.name])
    );
  }

  function normalizeValue(column, value) {
    const raw = String(value ?? "").trim();

    if (raw === "") {
      return "";
    }

    if (NUMERIC_COLUMNS.has(column)) {
      const asNumber = Number(raw.replace(",", "."));
      return Number.isFinite(asNumber) ? asNumber : raw;
    }

    return raw;
  }

  function nextCollectorNumber(value) {
    const n = Number(value);

    return Number.isFinite(n) && String(value ?? "").trim() !== ""
      ? n + 1
      : "";
  }

  function inputTypeAttrs(field) {
    return field.numeric
      ? `type="text" inputmode="numeric"`
      : `type="text"`;
  }

  function headerFieldHTML(field, value, eventId) {
    const common =
      `data-kind="event-field" ` +
      `data-event-id="${eventId}" ` +
      `data-field="${field.name}"`;

    if (field.select) {
      const options = field.select.map(option => {
        const selected =
          String(value ?? "") === String(option)
            ? "selected"
            : "";

        return `<option value="${escapeHtml(option)}" ${selected}>${escapeHtml(option || "—")}</option>`;
      }).join("");

      return `<select class="jb-select" ${common}>${options}</select>`;
    }

    if (field.textarea) {
      return `<textarea class="jb-textarea" ${common}>${escapeHtml(value)}</textarea>`;
    }

    return `<input class="jb-input" ${inputTypeAttrs(field)} value="${escapeHtml(value)}" ${common}>`;
  }

  function detailFieldControlHTML(field, value, dataset) {
    if (field.select) {
      const options = field.select.map(option => {
        const selected =
          String(value ?? "") === String(option)
            ? "selected"
            : "";

        return `<option value="${escapeHtml(option)}" ${selected}>${escapeHtml(option || "—")}</option>`;
      }).join("");

      return `<select class="jb-select" ${dataset}>${options}</select>`;
    }

    if (field.textarea || field.name === "notes") {
      return `<textarea class="jb-textarea" ${dataset}>${escapeHtml(value)}</textarea>`;
    }

    return `<input class="jb-input" ${inputTypeAttrs(field)} value="${escapeHtml(value)}" ${dataset}>`;
  }

  function formGridHTML(fields, values, eventId, specimenId = null) {
    return `
      <div class="jb-form-grid">
        ${fields.map(field => {
          const dataset = specimenId
            ? `data-kind="specimen-field" data-event-id="${eventId}" data-specimen-id="${specimenId}" data-field="${field.name}"`
            : `data-kind="event-field" data-event-id="${eventId}" data-field="${field.name}"`;

          const value = values[field.name] ?? "";

          const control = specimenId
            ? detailFieldControlHTML(field, value, dataset)
            : headerFieldHTML(field, value, eventId);

          return `
            <div class="jb-field" style="--span:${field.span || 3}">
              <label class="jb-label">
                ${escapeHtml(field.label)}
                ${helpIcon(field.name)}
              </label>
              ${control}
            </div>
          `;
        }).join("")}
      </div>
    `;
  }

  function render() {
    updateMetrics();
    validationPanel.classList.remove("is-visible");

    eventList.innerHTML =
      state.events
        .map((eventItem, index) => eventCardHTML(eventItem, index))
        .join("") +
      addEventCardHTML();
  }

  function eventCardHTML(eventItem, index) {
    return `
      <section class="jb-card jb-event-card" data-event-id="${eventItem.id}">
        <header class="jb-event-top">
          <div class="jb-event-name">
            <span class="jb-event-index">${index + 1}</span>

            <div>
              <h2>Collection event ${index + 1}</h2>
              <div class="jb-event-meta">
                ${escapeHtml(eventDate(eventItem))} · ${escapeHtml(eventPlace(eventItem))}
              </div>
            </div>
          </div>

          <div class="jb-event-summary">
            <div class="jb-summary-chip">
              <span>Espécimes</span>
              <strong>${eventItem.specimens.length}</strong>
            </div>

            <div class="jb-summary-chip">
              <span>Collector</span>
              <strong>${escapeHtml(eventItem.header.collector || "não preenchido")}</strong>
            </div>

            <div class="jb-summary-chip">
              <span>Cabeçalho</span>
              <strong>${headerProgress(eventItem)}%</strong>
            </div>
          </div>
        </header>

        <div class="jb-event-actions">
          <button
            class="jb-btn jb-btn--soft jb-btn--small"
            type="button"
            data-action="repeat-header"
            data-event-id="${eventItem.id}"
          >
            Repetir cabeçalho anterior
          </button>

          <button
            class="jb-btn jb-btn--soft jb-btn--small"
            type="button"
            data-action="duplicate-event-header"
            data-event-id="${eventItem.id}"
          >
            Duplicar cabeçalho
          </button>

          <button
            class="jb-btn jb-btn--soft jb-btn--small"
            type="button"
            data-action="toggle-event"
            data-event-id="${eventItem.id}"
          >
            ${eventItem.collapsed ? "Expandir" : "Recolher"}
          </button>

          <button
            class="jb-btn jb-btn--danger jb-btn--small"
            type="button"
            data-action="remove-event"
            data-event-id="${eventItem.id}"
          >
            ${
              pendingRemoveEventId === eventItem.id
                ? "Clique novamente para excluir"
                : "Remover evento"
            }
          </button>
        </div>

        ${eventItem.collapsed ? "" : `
          <div class="jb-section-header">
            <h3>Cabeçalho compartilhado</h3>
            ${helpIcon(
              "event-header",
              "Campos compartilhados: serão repetidos para todos os espécimes deste collection event."
            )}
          </div>

          ${formGridHTML(DAY_MAIN_FIELDS, eventItem.header, eventItem.id)}

          <details
            class="jb-details"
            ${DAY_EXTRA_FIELDS.some(field => isFilled(eventItem.header[field.name])) ? "open" : ""}
          >
            <summary>Campos avançados do cabeçalho</summary>
            <div style="padding-top:12px;">
              ${formGridHTML(DAY_EXTRA_FIELDS, eventItem.header, eventItem.id)}
            </div>
          </details>

          <div class="jb-specimen-header">
            <div class="jb-section-header" style="margin:0;">
              <h3>Espécimes</h3>
              ${helpIcon(
                "specimens",
                "Cada linha representa um espécime. Abra Detalhes para editar a ficha completa do táxon."
              )}
            </div>

            <div class="jb-specimen-actions">
              <button
                class="jb-btn jb-btn--soft jb-btn--small"
                type="button"
                data-action="add-specimen"
                data-event-id="${eventItem.id}"
              >
                Add specimen
              </button>

              <button
                class="jb-btn jb-btn--soft jb-btn--small"
                type="button"
                data-action="duplicate-last-specimen"
                data-event-id="${eventItem.id}"
              >
                Duplicar último
              </button>

              ${eventItem.specimens.length > 1 ? `
                <button
                  class="jb-btn jb-btn--plain jb-btn--small"
                  type="button"
                  data-action="sequence-numbers"
                  data-event-id="${eventItem.id}"
                >
                  Seguir sequência numérica
                </button>
              ` : ""}
            </div>
          </div>

          <div class="jb-grid-wrap">
            ${specimenGridHTML(eventItem)}
          </div>

          ${mobileSpecimenListHTML(eventItem)}

          ${eventItem.specimens
            .filter(specimen => specimen.detailsOpen)
            .map(specimen => specimenDetailHTML(eventItem.id, specimen))
            .join("")
          }
        `}
      </section>
    `;
  }

  function addEventCardHTML() {
    return `
      <section class="jb-card jb-add-event-card">
        <div>
          <h3>Novo collection event</h3>
          <p>Crie outro cabeçalho para uma nova localidade, data ou combinação de coleta.</p>
        </div>

        <button
          class="jb-btn jb-btn--accent"
          type="button"
          data-action="add-event-after-last"
        >
          Add collection event
        </button>
      </section>
    `;
  }

  function mobileSpecimenListHTML(eventItem) {
    return `
      <div class="jb-mobile-specimen-list">
        ${eventItem.specimens
          .map((specimen, index) =>
            mobileSpecimenCardHTML(eventItem.id, specimen, index)
          )
          .join("")
        }
      </div>
    `;
  }

  function mobileSpecimenCardHTML(eventId, specimen, index) {
    const taxon =
      [specimen.family, specimen.genus, specimen.sp1]
        .filter(isFilled)
        .join(" ") ||
      "Táxon não preenchido";

    return `
      <article class="jb-mobile-specimen-card ${specimen.done ? "jb-row-done" : ""}">
        <div class="jb-mobile-specimen-top">
          <div>
            <h4 class="jb-mobile-specimen-title">Espécime ${index + 1}</h4>
            <p class="jb-mobile-specimen-subtitle">
              ${escapeHtml(taxon)} · Nº ${escapeHtml(specimen.number || "—")}
            </p>
          </div>

          <label class="jb-check-wrap" title="Marcar como revisado">
            <input
              class="jb-check"
              type="checkbox"
              ${specimen.done ? "checked" : ""}
              data-action="toggle-specimen-check"
              data-event-id="${eventId}"
              data-specimen-id="${specimen.id}"
            >
          </label>
        </div>

        <div class="jb-mobile-specimen-fields">
          ${["number", "family", "genus", "sp1"].map(name => {
            const field = FIELD_BY_NAME[name];

            return `
              <div class="jb-field" style="--span:3">
                <label class="jb-label">
                  ${escapeHtml(field.label)}
                  ${helpIcon(field.name)}
                </label>

                ${detailFieldControlHTML(
                  field,
                  specimen[name],
                  `data-kind="specimen-field" data-event-id="${eventId}" data-specimen-id="${specimen.id}" data-field="${field.name}"`
                )}
              </div>
            `;
          }).join("")}
        </div>

        <div class="jb-mobile-actions">
          <button
            class="jb-btn jb-btn--soft jb-btn--small"
            type="button"
            data-action="toggle-specimen-details"
            data-event-id="${eventId}"
            data-specimen-id="${specimen.id}"
          >
            ${specimen.detailsOpen ? "Fechar detalhes" : "Abrir detalhes"}
          </button>

          <button
            class="jb-btn jb-btn--danger jb-btn--small"
            type="button"
            data-action="remove-specimen"
            data-event-id="${eventId}"
            data-specimen-id="${specimen.id}"
          >
            Remover
          </button>
        </div>
      </article>
    `;
  }

  function specimenGridHTML(eventItem) {
    const columns = [
      "50px",
      ...SPECIMEN_FIELDS.map(
        field => `${state.columnWidths[field.name] || field.width}px`
      ),
      "172px"
    ];

    return `
      <div class="jb-grid" style="grid-template-columns:${columns.join(" ")};">
        <div class="jb-grid-head">
          <div class="jb-th">
            <div class="jb-head-box">
              <div class="jb-head-top">
                <span class="jb-head-name">
                  <strong>OK</strong>
                  ${helpIcon("check", "Marque quando o espécime estiver revisado.")}
                </span>
              </div>
            </div>
          </div>

          ${SPECIMEN_FIELDS.map(field => `
            <div class="jb-th">
              <div class="jb-head-box">
                <div class="jb-head-top">
                  <span class="jb-head-name">
                    <strong>${escapeHtml(field.label)}</strong>
                    ${helpIcon(field.name)}
                  </span>

                  <span class="jb-width-tools">
                    <button
                      class="jb-width-btn"
                      type="button"
                      data-action="narrow-col"
                      data-col="${field.name}"
                      aria-label="Diminuir largura"
                    >−</button>

                    <button
                      class="jb-width-btn"
                      type="button"
                      data-action="widen-col"
                      data-col="${field.name}"
                      aria-label="Aumentar largura"
                    >+</button>
                  </span>
                </div>
              </div>
            </div>
          `).join("")}

          <div class="jb-th">
            <div class="jb-head-box">
              <div class="jb-head-top">
                <span class="jb-head-name">
                  <strong>Ações</strong>
                </span>
              </div>
            </div>
          </div>
        </div>

        ${eventItem.specimens
          .map(specimen => specimenRowHTML(eventItem.id, specimen))
          .join("")
        }
      </div>
    `;
  }

  function specimenRowHTML(eventId, specimen) {
    return `
      <div
        class="jb-grid-row ${specimen.done ? "jb-row-done" : ""}"
        data-specimen-id="${specimen.id}"
      >
        <div class="jb-td">
          <div class="jb-check-wrap">
            <input
              class="jb-check"
              type="checkbox"
              ${specimen.done ? "checked" : ""}
              data-action="toggle-specimen-check"
              data-event-id="${eventId}"
              data-specimen-id="${specimen.id}"
            >
          </div>
        </div>

        ${SPECIMEN_FIELDS.map(field => `
          <div class="jb-td">
            <div class="jb-input-wrap ${isFilled(specimen[field.name]) ? "is-filled" : ""}">
              ${specimenCellControl(field, specimen, eventId)}
            </div>
          </div>
        `).join("")}

        <div class="jb-td">
          <div class="jb-row-actions">
            <button
              class="jb-btn jb-btn--soft"
              type="button"
              data-action="toggle-specimen-details"
              data-event-id="${eventId}"
              data-specimen-id="${specimen.id}"
            >
              ${specimen.detailsOpen ? "Fechar" : "Detalhes"}
            </button>

            <button
              class="jb-btn jb-btn--danger"
              type="button"
              data-action="remove-specimen"
              data-event-id="${eventId}"
              data-specimen-id="${specimen.id}"
            >
              Remover
            </button>
          </div>
        </div>
      </div>
    `;
  }

  function specimenCellControl(field, specimen, eventId) {
    const common =
      `data-kind="specimen-field" ` +
      `data-event-id="${eventId}" ` +
      `data-specimen-id="${specimen.id}" ` +
      `data-field="${field.name}"`;

    if (field.textarea) {
      return `<textarea class="jb-cell-textarea" ${common}>${escapeHtml(specimen[field.name])}</textarea>`;
    }

    return `<input class="jb-cell-input" ${inputTypeAttrs(field)} value="${escapeHtml(specimen[field.name])}" ${common}>`;
  }

  function specimenDetailHTML(eventId, specimen) {
    const specimenLabel =
      [specimen.family, specimen.genus, specimen.sp1]
        .filter(isFilled)
        .join(" ") ||
      "táxon sem identificação";

    return `
      <section class="jb-detail-panel" data-detail-for="${specimen.id}">
        <div class="jb-detail-top">
          <div>
            <h3>Ficha do espécime</h3>
            <p>
              ${escapeHtml(specimenLabel)}
              · número ${escapeHtml(specimen.number || "não preenchido")}
            </p>
          </div>

          <button
            class="jb-btn jb-btn--soft jb-btn--small"
            type="button"
            data-action="toggle-specimen-details"
            data-event-id="${eventId}"
            data-specimen-id="${specimen.id}"
          >
            Fechar ficha
          </button>
        </div>

        ${DETAIL_GROUPS.map(group => `
          <div class="jb-detail-section">
            <h4>${escapeHtml(group.title)}</h4>

            ${formGridHTML(
              group.fields.map(name => FIELD_BY_NAME[name]),
              specimen,
              eventId,
              specimen.id
            )}
          </div>
        `).join("")}

        <div class="jb-detail-actions">
          <button
            class="jb-btn jb-btn--soft"
            type="button"
            data-action="toggle-specimen-details"
            data-event-id="${eventId}"
            data-specimen-id="${specimen.id}"
          >
            Voltar para tabela
          </button>

          <button
            class="jb-btn jb-btn--primary"
            type="button"
            data-action="confirm-taxon"
            data-event-id="${eventId}"
            data-specimen-id="${specimen.id}"
          >
            Confirmar táxon
          </button>
        </div>
      </section>
    `;
  }

  function updateMetrics() {
    const events = state.events.length;

    const specimens = state.events.reduce(
      (total, eventItem) => total + eventItem.specimens.length,
      0
    );

    const headerPct = events
      ? Math.round(
          state.events.reduce(
            (sum, eventItem) => sum + headerProgress(eventItem),
            0
          ) / events
        )
      : 0;

    const metricEvents = document.getElementById("metric-events");
    const metricSpecimens = document.getElementById("metric-specimens");
    const metricHeaders = document.getElementById("metric-headers");

    if (metricEvents) metricEvents.textContent = String(events);
    if (metricSpecimens) metricSpecimens.textContent = String(specimens);
    if (metricHeaders) metricHeaders.textContent = `${headerPct}%`;
  }

  function toast(message) {
    if (!toastEl) return;

    toastEl.textContent = message;
    toastEl.classList.add("is-visible");

    clearTimeout(toastEl._timer);

    toastEl._timer = setTimeout(
      () => toastEl.classList.remove("is-visible"),
      2200
    );
  }

  function autosave() {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
    } catch (_error) {
      // Alguns navegadores limitam localStorage em file://.
    }
  }

  function saveDraft() {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(state));

      toast(
        t(
          "toast.draftSaved",
          {},
          "Rascunho salvo neste navegador."
        )
      );
    } catch (_error) {
      toast("O navegador não permitiu salvar o rascunho local.");
    }
  }

  function loadDraft() {
    let saved = null;

    try {
      saved = localStorage.getItem(STORAGE_KEY);
    } catch (_error) {
      toast("O navegador não permitiu acessar o rascunho local.");
      return;
    }

    if (!saved) {
      toast(
        t(
          "toast.noDraft",
          {},
          "Nenhum rascunho salvo foi encontrado."
        )
      );
      return;
    }

    try {
      const parsed = JSON.parse(saved);

      state.events = (parsed.events || parsed.days || []).map(createEvent);

      state.columnWidths = {
        ...defaultColumnWidths,
        ...(parsed.columnWidths || {})
      };

      if (!state.events.length) {
        state.events = [createEvent({ country: "Brasil" })];
      }

      render();

      toast(
        t(
          "toast.draftLoaded",
          {},
          "Rascunho carregado."
        )
      );
    } catch (_error) {
      toast(
        t(
          "toast.draftLoadError",
          {},
          "Não foi possível carregar o rascunho."
        )
      );
    }
  }

  function addEvent(position = "end", seed = null) {
    const eventItem = createEvent(seed || { country: "Brasil" });

    if (position === "start") {
      state.events.unshift(eventItem);
    } else {
      state.events.push(eventItem);
    }

    render();
    autosave();
  }

  function removeEvent(eventId) {
    if (pendingRemoveEventId !== eventId) {
      pendingRemoveEventId = eventId;

      window.clearTimeout(pendingRemoveTimer);

      pendingRemoveTimer = window.setTimeout(() => {
        if (pendingRemoveEventId === eventId) {
          pendingRemoveEventId = null;
          render();
        }
      }, 5000);

      render();
      toast("Clique novamente em “Remover evento” para confirmar a exclusão.");
      return;
    }

    window.clearTimeout(pendingRemoveTimer);
    pendingRemoveTimer = null;
    pendingRemoveEventId = null;

    state.events = state.events.filter(
      eventItem => eventItem.id !== eventId
    );

    if (!state.events.length) {
      state.events = [createEvent({ country: "Brasil" })];
    }

    render();
    autosave();
    toast("Evento de coleta removido.");
  }

  function addSpecimen(eventId) {
    const eventItem = findEvent(eventId);
    if (!eventItem) return;

    eventItem.specimens.push(
      createSpecimen({
        number: nextCollectorNumber(
          eventItem.specimens[eventItem.specimens.length - 1]?.number
        )
      })
    );

    render();
    autosave();
  }

  function duplicateLastSpecimen(eventId) {
    const eventItem = findEvent(eventId);

    if (!eventItem || !eventItem.specimens.length) {
      return;
    }

    const last = eventItem.specimens[eventItem.specimens.length - 1];

    eventItem.specimens.push(
      createSpecimen({
        ...last,
        id: undefined,
        detailsOpen: false,
        done: false,
        number: nextCollectorNumber(last.number)
      })
    );

    render();
    autosave();
  }

  function removeSpecimen(eventId, specimenId) {
    const eventItem = findEvent(eventId);
    if (!eventItem) return;

    eventItem.specimens = eventItem.specimens.filter(
      specimen => specimen.id !== specimenId
    );

    if (!eventItem.specimens.length) {
      eventItem.specimens = [createSpecimen()];
    }

    render();
    autosave();
  }

  function repeatHeader(eventId) {
    const idx = state.events.findIndex(
      eventItem => eventItem.id === eventId
    );

    if (idx <= 0) {
      toast(
        t(
          "toast.noPreviousHeader",
          {},
          "Não há cabeçalho anterior para repetir."
        )
      );
      return;
    }

    const current = state.events[idx];
    const previous = state.events[idx - 1];

    COPY_HEADER_FIELDS.forEach(field => {
      current.header[field] = previous.header[field] ?? "";
    });

    render();
    autosave();

    toast(
      t(
        "toast.headerRepeated",
        {},
        "Cabeçalho anterior repetido."
      )
    );
  }

  function duplicateEventHeader(eventId) {
    const eventItem = findEvent(eventId);
    if (!eventItem) return;

    const idx = state.events.findIndex(
      item => item.id === eventId
    );

    const clone = createEvent({
      header: { ...eventItem.header }
    });

    clone.header.colldd = "";
    clone.header.collmm = "";
    clone.header.collyy = "";

    state.events.splice(idx + 1, 0, clone);

    render();
    autosave();
  }

  function sequenceNumbers(eventId) {
    const eventItem = findEvent(eventId);
    if (!eventItem) return;

    let start = Number(
      eventItem.specimens.find(
        specimen => isFilled(specimen.number)
      )?.number || 1
    );

    if (!Number.isFinite(start)) {
      start = 1;
    }

    eventItem.specimens.forEach((specimen, index) => {
      specimen.number = String(start + index);
    });

    render();
    autosave();

    toast(
      t(
        "toast.sequenceApplied",
        {},
        "Sequência numérica aplicada."
      )
    );
  }

  function setWidth(column, delta) {
    const field = SPECIMEN_FIELDS.find(
      item => item.name === column
    );

    if (!field) return;

    const current =
      state.columnWidths[column] ||
      field.width;

    state.columnWidths[column] = Math.max(
      field.min,
      Math.min(field.max, current + delta)
    );

    render();
    autosave();
  }

  function flattenRows() {
    const rows = [];

    state.events.forEach(eventItem => {
      eventItem.specimens
        .filter(specimen => !isSpecimenEmpty(specimen))
        .forEach(specimen => {
          const row = {};

          JABOT_COLUMNS.forEach(column => {
            row[column] = "";
          });

          Object.entries(eventItem.header).forEach(([key, value]) => {
            if (key in row) {
              row[key] = normalizeValue(key, value);
            }
          });

          Object.entries(specimen).forEach(([key, value]) => {
            if (key in row) {
              row[key] = normalizeValue(key, value);
            }
          });

          rows.push(row);
        });
    });

    return rows;
  }

function collectionFileBaseName() {
  const rows = flattenRows();

  const firstEvent = state.events.find(eventItem =>
    isFilled(eventItem.header.collector)
  );

  const collector = firstEvent?.header.collector || "Coleta";

function formatCollectorName(value) {
  const text = String(value || "")
    .trim()
    .replace(/\s+/g, " ");

  if (!text) {
    return "Coleta";
  }

  if (text.includes(",")) {
    const parts = text.split(",");

    const surname = parts[0]
      .trim()
      .replace(/[^A-Za-zÀ-ÿ'-]/g, "");

    const namesPart = parts
      .slice(1)
      .join(" ")
      .trim();

    const firstInitial = namesPart
      .replace(/[^A-Za-zÀ-ÿ]/g, "")
      .charAt(0)
      .toUpperCase();

    return `${firstInitial}${surname}`;
  }

  const words = text
    .split(/\s+/)
    .filter(Boolean);

  if (words.length === 1) {
    return words[0]
      .replace(/[^A-Za-zÀ-ÿ0-9'-]/g, "");
  }

  const firstName = words[0]
    .replace(/[^A-Za-zÀ-ÿ]/g, "");

  const surname = words[words.length - 1]
    .replace(/[^A-Za-zÀ-ÿ'-]/g, "");

  const firstInitial = firstName
    .charAt(0)
    .toUpperCase();

  return `${firstInitial}${surname}`;
}

  const collectorName = formatCollectorName(collector);

  const numbers = rows
    .map(row => String(row.number ?? "").trim())
    .filter(Boolean);

  if (!numbers.length) {
    return collectorName;
  }

  const firstNumber = numbers[0];
  const lastNumber = numbers[numbers.length - 1];

  if (firstNumber === lastNumber) {
    return `${collectorName}${firstNumber}`;
  }

  return `${collectorName}${firstNumber}-${lastNumber}`;
}

  function validateRows(rows) {
    const warnings = [];

    if (!rows.length) {
      warnings.push(
        t(
          "validation.noSpecimens",
          {},
          "Nenhum espécime preenchido foi encontrado."
        )
      );
    }

    state.events.forEach((eventItem, eventIndex) => {
      const label = t(
        "validation.eventLabel",
        { n: eventIndex + 1 },
        `Collection event ${eventIndex + 1}`
      );

      if (
        !eventItem.header.colldd ||
        !eventItem.header.collmm ||
        !eventItem.header.collyy
      ) {
        warnings.push(
          t(
            "validation.incompleteDate",
            { label },
            `${label}: data incompleta.`
          )
        );
      }

      if (!eventItem.header.collector) {
        warnings.push(
          t(
            "validation.noCollector",
            { label },
            `${label}: collector não preenchido.`
          )
        );
      }

      const filled = eventItem.specimens.filter(
        specimen => !isSpecimenEmpty(specimen)
      );

      if (!filled.length) {
        warnings.push(
          t(
            "validation.noSpecimenInEvent",
            { label },
            `${label}: nenhum espécime preenchido.`
          )
        );
      }

      filled.forEach((specimen, specimenIndex) => {
        const n = specimenIndex + 1;

        if (!specimen.number) {
          warnings.push(
            t(
              "validation.noNumber",
              { label, n },
              `${label}, espécime ${n}: número de coleta não preenchido.`
            )
          );
        }
      });
    });

    return warnings;
  }

  function showValidation(warnings) {
    validationPanel.innerHTML = `
      <strong>
        ${escapeHtml(
          t(
            "validation.title",
            {},
            "Verificação antes da exportação"
          )
        )}
      </strong>

      <ul>
        ${warnings
          .map(warning => `<li>${escapeHtml(warning)}</li>`)
          .join("")
        }
      </ul>
    `;

    validationPanel.classList.add("is-visible");

    validationPanel.scrollIntoView({
      behavior: "smooth",
      block: "center"
    });
  }

  function ensureSheetJS() {
    if (!window.XLSX) {
      window.alert(
        t(
          "alert.xlsxMissing",
          {},
          "A biblioteca XLSX não carregou. Confirme se vendor/xlsx.full.min.js está na pasta fieldbook."
        )
      );

      return false;
    }

    return true;
  }

  function setWorksheetWidths(ws) {
    ws["!cols"] = JABOT_COLUMNS.map(column => ({
      wch: Math.max(10, Math.min(30, column.length + 4))
    }));
  }

  function downloadEmptyWorkbook() {
    if (!ensureSheetJS()) return;

    const wb = XLSX.utils.book_new();
    const ws = XLSX.utils.aoa_to_sheet([JABOT_COLUMNS]);

    setWorksheetWidths(ws);

    XLSX.utils.book_append_sheet(wb, ws, "Plan1");
    XLSX.writeFile(wb, "BasePadrao_JabotEspecime_vazia.xlsx");

    toast(
      t(
        "toast.emptyDownloaded",
        {},
        "Planilha vazia padrão Jabot baixada."
      )
    );
  }

  function exportFilledWorkbook() {
    if (!ensureSheetJS()) return;

    const rows = flattenRows();
    const warnings = validateRows(rows);

    if (warnings.length) {
      showValidation(warnings);

      if (!rows.length) {
        pendingExportWithWarnings = false;
        toast("Preencha pelo menos um espécime antes de gerar a planilha.");
        return;
      }

      if (!pendingExportWithWarnings) {
        pendingExportWithWarnings = true;
        toast("Há avisos de preenchimento. Clique novamente em “Gerar planilha Jabot” para exportar mesmo assim.");
        return;
      }
    }

    pendingExportWithWarnings = false;

    const wb = XLSX.utils.book_new();

    const ws = XLSX.utils.json_to_sheet(
      rows,
      {
        header: JABOT_COLUMNS,
        skipHeader: false
      }
    );

    setWorksheetWidths(ws);

    XLSX.utils.book_append_sheet(wb, ws, "Plan1");
    XLSX.writeFile(
  wb,
  `${collectionFileBaseName()}.xlsx`
);

    autosave();

    toast(
      t(
        "toast.exported",
        { n: rows.length },
        `Planilha gerada com ${rows.length} linha${rows.length === 1 ? "" : "s"}.`
      )
    );
  }


  function fieldbookPdfLocale() {
    const language =
      window.JabotI18n &&
      typeof window.JabotI18n.getLanguage === "function"
        ? window.JabotI18n.getLanguage()
        : "pt";

    if (language === "en") {
      return {
        collectionEvent: "Collection event",
        dateMissing: "Collection date not entered",
        localityMissing: "Locality not entered",
        collector: "Collector",
        additionalCollectors: "Additional collectors",
        noCollector: "not entered",
        noSpecimens: "No completed specimens were found in this collection event.",
        printTitle: "jabotR Field notebook",
        habit: "Habit",
        height: "Height",
        flowers: "Flowers",
        fruits: "Fruits",
        stem: "Stem"
      };
    }

    return {
      collectionEvent: "Collection event",
      dateMissing: "Data da coleta não preenchida",
      localityMissing: "Localidade não preenchida",
      collector: "Coletor",
      additionalCollectors: "Coletores adicionais",
      noCollector: "não preenchido",
      noSpecimens: "Nenhum espécime preenchido foi encontrado neste evento de coleta.",
      printTitle: "jabotR Caderno de campo",
      habit: "Hábito",
      height: "Altura",
      flowers: "Flores",
      fruits: "Frutos",
      stem: "Fuste"
    };
  }

  function formatFieldbookDate(header, locale) {
    const day = Number(header.colldd);
    const month = Number(header.collmm);
    const year = Number(header.collyy);

    if (
      !Number.isInteger(day) ||
      !Number.isInteger(month) ||
      month < 1 ||
      month > 12
    ) {
      return locale.dateMissing;
    }

    const language =
      window.JabotI18n &&
      typeof window.JabotI18n.getLanguage === "function"
        ? window.JabotI18n.getLanguage()
        : "pt";

    const monthNames =
      language === "en"
        ? [
            "January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"
          ]
        : [
            "janeiro", "fevereiro", "março", "abril", "maio", "junho",
            "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
          ];

    if (language === "en") {
      return `${monthNames[month - 1]} ${day}${Number.isInteger(year) ? `, ${year}` : ""}`;
    }

    return `${day} de ${monthNames[month - 1]}${Number.isInteger(year) ? ` de ${year}` : ""}`;
  }

function fieldbookLocation(header, locale) {
  const parts = [
    header.uc,
    header.gazetteer,
    header.locnotes,
    header.habitat,
    header.minorarea,
    header.majorarea,
    header.country
  ]
    .map(value => String(value ?? "").trim())
    .filter(Boolean);

  const unique = parts.filter(
    (value, index, array) =>
      array.findIndex(
        item =>
          item.toLocaleLowerCase() ===
          value.toLocaleLowerCase()
      ) === index
  );

  return unique.length
    ? unique.join(", ")
    : locale.localityMissing;
}
  function scientificNameHTML(specimen) {
    const pieces = [];

    const genus = String(specimen.genus ?? "").trim();
    const cf = String(specimen.cf ?? "").trim();
    const sp1 = String(specimen.sp1 ?? "").trim();
    const author1 = String(specimen.author1 ?? "").trim();
    const rank1 = String(specimen.rank1 ?? "").trim();
    const sp2 = String(specimen.sp2 ?? "").trim();
    const author2 = String(specimen.author2 ?? "").trim();
    const rank2 = String(specimen.rank2 ?? "").trim();
    const sp3 = String(specimen.sp3 ?? "").trim();
    const author3 = String(specimen.author3 ?? "").trim();

    if (genus) pieces.push(`<em>${escapeHtml(genus)}</em>`);
    if (cf) pieces.push(`<span>${escapeHtml(cf)}</span>`);
    if (sp1) pieces.push(`<em>${escapeHtml(sp1)}</em>`);
    if (author1) pieces.push(`<span>${escapeHtml(author1)}</span>`);
    if (rank1) pieces.push(`<span>${escapeHtml(rank1)}</span>`);
    if (sp2) pieces.push(`<em>${escapeHtml(sp2)}</em>`);
    if (author2) pieces.push(`<span>${escapeHtml(author2)}</span>`);
    if (rank2) pieces.push(`<span>${escapeHtml(rank2)}</span>`);
    if (sp3) pieces.push(`<em>${escapeHtml(sp3)}</em>`);
    if (author3) pieces.push(`<span>${escapeHtml(author3)}</span>`);

    return pieces.join(" ");
  }

  function specimenDescriptionHTML(specimen, locale) {
    const parts = [];

    const notes = String(specimen.notes ?? "").trim();

    if (notes) {
      parts.push(
        escapeHtml(notes).replace(/\n/g, "<br>")
      );
    }

    const structured = [];

    if (isFilled(specimen.habito)) {
      structured.push(
        `${locale.habit}: ${escapeHtml(specimen.habito)}`
      );
    }

    if (isFilled(specimen.altura)) {
      structured.push(
        `${locale.height}: ${escapeHtml(specimen.altura)}${
          isFilled(specimen.unidmedaltura)
            ? ` ${escapeHtml(specimen.unidmedaltura)}`
            : ""
        }`
      );
    }

    if (isFilled(specimen.flor)) {
      structured.push(
        `${locale.flowers}: ${escapeHtml(specimen.flor)}`
      );
    }

    if (isFilled(specimen.fruto)) {
      structured.push(
        `${locale.fruits}: ${escapeHtml(specimen.fruto)}`
      );
    }

    if (isFilled(specimen.fuste)) {
      structured.push(
        `${locale.stem}: ${escapeHtml(specimen.fuste)}`
      );
    }

    if (structured.length) {
      parts.push(structured.join("; "));
    }

    return parts.join(" ");
  }

  function fieldbookSpecimenHTML(specimen, locale) {
    const number = isFilled(specimen.number)
      ? escapeHtml(specimen.number)
      : "—";

    const family = isFilled(specimen.family)
      ? escapeHtml(specimen.family)
      : "";

    const scientificName = scientificNameHTML(specimen);
    const description = specimenDescriptionHTML(specimen, locale);

    const identity = [family, scientificName]
      .filter(Boolean)
      .join(" — ");

    return `
      <div class="fb-specimen">
        <div class="fb-number">${number}</div>

        <div class="fb-specimen-text">
          ${identity ? `<span class="fb-identity">${identity}</span>` : ""}
          ${
            description
              ? `${identity ? " — " : ""}<span class="fb-description">${description}</span>`
              : ""
          }
        </div>
      </div>
    `;
  }

function fieldbookEventHTML(eventItem, eventIndex, locale) {
  const specimens = eventItem.specimens.filter(
    specimen => !isSpecimenEmpty(specimen)
  );

  const collector =
    String(eventItem.header.collector ?? "").trim() ||
    locale.noCollector;

  const additionalCollectors =
    String(eventItem.header.addcoll ?? "").trim();

  const latitude =
    String(eventItem.header.latitude ?? "").trim();

  const longitude =
    String(eventItem.header.longitude ?? "").trim();

  const location =
    String(fieldbookLocation(eventItem.header, locale) || "").trim();

  const locationWithCoordinates =
    latitude && longitude
      ? `${location}, ${latitude}, ${longitude}`
      : location;

  return `
    <section class="fb-event">
      <header class="fb-event-header">
        <div class="fb-event-number">
          ${escapeHtml(locale.collectionEvent)} ${eventIndex + 1}
        </div>

        <div class="fb-date">
          ${escapeHtml(formatFieldbookDate(eventItem.header, locale))}
        </div>

        <div class="fb-location">
          ${escapeHtml(locationWithCoordinates)}
        </div>

        <div class="fb-collectors">
          <div>
            <strong>${escapeHtml(locale.collector)} —</strong>
            ${escapeHtml(collector)}
          </div>

          ${
            additionalCollectors
              ? `
                  <div>
                    <strong>${escapeHtml(locale.additionalCollectors)} —</strong>
                    ${escapeHtml(additionalCollectors)}
                  </div>
                `
              : ""
          }
        </div>
      </header>

      <div class="fb-specimen-list">
        ${
          specimens.length
            ? specimens
                .map(specimen => fieldbookSpecimenHTML(specimen, locale))
                .join("")
            : `<p class="fb-empty">${escapeHtml(locale.noSpecimens)}</p>`
        }
      </div>
    </section>
  `;
}

  function buildFieldbookPrintHTML() {
    const locale = fieldbookPdfLocale();

    const stickerUrl = new URL(
      "jabotr_hex_sticker.png",
      window.location.href
    ).href;

    const leafUrl = new URL(
      "jabot_original_logo.png",
      window.location.href
    ).href;

    const content = state.events
      .map(
        (eventItem, index) =>
          fieldbookEventHTML(eventItem, index, locale)
      )
      .join("");

    const language =
      window.JabotI18n &&
      typeof window.JabotI18n.getLanguage === "function"
        ? window.JabotI18n.getLanguage()
        : "pt";

    return `<!DOCTYPE html>
<html lang="${language === "en" ? "en" : "pt-BR"}">
<head>
  <meta charset="UTF-8">
  <title>${escapeHtml(collectionFileBaseName())}</title>

  <style>
    /*
      Página sem margem interna do navegador.
      A moldura ocupa a periferia da folha e o texto fica
      exclusivamente dentro da área branca.
    */
    @page {
      size: A4;
      margin: 0;
    }

    *,
    *::before,
    *::after {
      box-sizing: border-box;
    }

    html {
      -webkit-print-color-adjust: exact;
      print-color-adjust: exact;
    }

    html,
    body {
      margin: 0;
      padding: 0;
      background: #ffffff;
      color: #111111;
    }

    body {
      font-family: Calibri, "Segoe UI", Arial, sans-serif;
      font-size: 11pt;
      line-height: 1.32;
    }

    /*
      Moldura verde quadriculada.
      O padrão é desenhado por dois gradientes lineares,
      um horizontal e outro vertical.
    */
    .fb-page-frame {
      position: fixed;
      z-index: 0;
      inset: 0;

      background-color: #a8cb84;
      background-image:
        linear-gradient(
          rgba(255, 255, 255, 0.33) 1px,
          transparent 1px
        ),
        linear-gradient(
          90deg,
          rgba(255, 255, 255, 0.33) 1px,
          transparent 1px
        );
      background-size: 5mm 5mm;

      pointer-events: none;
    }

    /*
      Janela branca interna.
      A faixa quadriculada fica visível em todo o contorno,
      como uma moldura de papel decorado.
    */
    .fb-page-white {
      position: fixed;
      z-index: 1;

      top: 9mm;
      right: 9mm;
      bottom: 9mm;
      left: 9mm;

      background: #ffffff;
      border-radius: 1.4mm;

      pointer-events: none;
    }

    /*
      Folha do jabotR como marca-d'água cinza,
      sempre dentro da área branca.
    */
    .fb-watermark {
      position: fixed;
      z-index: 2;

      left: 50%;
      top: 53%;

      width: 92mm;
      max-height: 142mm;

      transform: translate(-50%, -50%);
      object-fit: contain;

      opacity: 0.035;
      filter: grayscale(1);

      pointer-events: none;
    }

    /*
      Todo o conteúdo recebe folga adicional em relação à
      borda interna da moldura. Assim nenhum texto toca a faixa verde.
    */
    .fb-content {
      position: relative;
      z-index: 3;

      padding:
        14mm
        15mm
        14mm
        15mm;
    }

    /*
      Logo somente na abertura do caderno.
    */
    .fb-document-brand {
      display: flex;
      justify-content: flex-end;
      align-items: flex-start;

      min-height: 17mm;
      margin: 0 0 1.5mm;
    }

    .fb-document-logo {
      width: 20mm;
      height: auto;
      display: block;
      object-fit: contain;
    }

    /*
      Eventos em fluxo contínuo.
      Não existe quebra de página obrigatória entre eles.
    */
    .fb-event {
      margin: 0 0 5mm;
      padding: 0;

      break-before: auto;
      page-break-before: auto;
      break-inside: auto;
      page-break-inside: auto;
    }

    .fb-event + .fb-event {
      margin-top: 5mm;
    }

    .fb-event-header {
      margin: 0 0 2.6mm;
      padding: 0;

      break-after: avoid;
      page-break-after: avoid;
    }

    .fb-event-number {
      margin: 0 0 1mm;

      color: #4f7648;
      font-size: 9.5pt;
      font-weight: 700;
      letter-spacing: 0.045em;
      text-transform: uppercase;
    }

    .fb-date {
      margin: 0 0 0.7mm;

      font-size: 12pt;
      font-weight: 700;
    }

    .fb-location {
      margin: 0 0 2mm;

      font-size: 11pt;
      font-weight: 700;
      line-height: 1.28;

      text-transform: none;
    }

    .fb-collectors {
      display: grid;
      gap: 0.6mm;

      margin: 0;

      font-size: 10.8pt;
    }

    /*
      Coletas próximas, uma logo abaixo da outra.
    */
    .fb-specimen-list {
      display: grid;
      gap: 1.4mm;

      margin: 0;
      padding: 0;
    }

    .fb-specimen {
      display: grid;
      grid-template-columns: 10mm minmax(0, 1fr);
      gap: 2mm;
      align-items: start;

      margin: 0;

      break-inside: avoid;
      page-break-inside: avoid;
    }

    .fb-number {
      font-size: 11.3pt;
      font-weight: 700;
      line-height: 1.32;
    }

    .fb-specimen-text {
      min-width: 0;
    }

    .fb-identity {
      line-height: 1.32;
    }

    .fb-identity em {
      font-style: italic;
    }

    .fb-description {
      color: #171717;
    }

    .fb-empty {
      margin: 0;
      color: #666666;
      font-style: italic;
    }

    /*
      Na pré-visualização HTML mantemos a mesma proporção visual.
    */
    @media screen {
      body {
        width: 210mm;
        min-height: 297mm;
        margin: 0 auto;
      }
    }
  </style>
</head>

<body>
  <div class="fb-page-frame" aria-hidden="true"></div>
  <div class="fb-page-white" aria-hidden="true"></div>

  <img
    class="fb-watermark"
    src="${leafUrl}"
    alt=""
    aria-hidden="true"
  >

  <main class="fb-content">
    <div class="fb-document-brand">
      <img
        class="fb-document-logo"
        src="${stickerUrl}"
        alt=""
        aria-hidden="true"
      >
    </div>

    ${content}
  </main>
</body>
</html>`;
  }

  function exportFieldbookPDF() {
    const iframe = document.createElement("iframe");

    iframe.setAttribute("aria-hidden", "true");

    Object.assign(iframe.style, {
      position: "fixed",
      right: "0",
      bottom: "0",
      width: "1px",
      height: "1px",
      opacity: "0",
      border: "0",
      pointerEvents: "none"
    });

    document.body.appendChild(iframe);

    const printDocument = iframe.contentDocument;
    const printWindow = iframe.contentWindow;

    if (!printDocument || !printWindow) {
      iframe.remove();
      toast("Não foi possível preparar o caderno de campo para impressão.");
      return;
    }

    const cleanup = () => {
      window.setTimeout(() => {
        iframe.remove();
      }, 300);
    };

    printWindow.addEventListener(
      "afterprint",
      cleanup,
      { once: true }
    );

    try {
      printDocument.open();
      printDocument.write(buildFieldbookPrintHTML());
      printDocument.close();

      /*
        A impressão é solicitada ainda dentro do clique do usuário.
        Assim o navegador não perde a autorização para abrir a caixa
        de impressão, como acontecia com os setTimeout anteriores.
      */
      printWindow.focus();
      printWindow.print();
    } catch (error) {
      console.error("jabotR Field Book PDF:", error);
      iframe.remove();
      toast("Não foi possível abrir a impressão do caderno de campo.");
    }
  }


  document.addEventListener("input", event => {
    const el = event.target;

    if (!el.matches("[data-kind]")) {
      return;
    }

    const eventItem = findEvent(el.dataset.eventId);

    if (!eventItem) {
      return;
    }

    const field = el.dataset.field;

    if (el.dataset.kind === "event-field") {
      eventItem.header[field] = el.value;
    }

    if (el.dataset.kind === "specimen-field") {
      const specimen = findSpecimen(
        eventItem,
        el.dataset.specimenId
      );

      if (specimen) {
        specimen[field] = el.value;
      }

      const wrap = el.closest(".jb-input-wrap");

      if (wrap) {
        wrap.classList.toggle(
          "is-filled",
          isFilled(el.value)
        );
      }
    }

    pendingExportWithWarnings = false;
    updateMetrics();
    autosave();
  });

  document.addEventListener("change", event => {
    const checkbox = event.target.closest(".jb-check");

    if (!checkbox) {
      return;
    }

    const eventItem = findEvent(
      checkbox.dataset.eventId
    );

    const specimen = findSpecimen(
      eventItem,
      checkbox.dataset.specimenId
    );

    if (specimen) {
      specimen.done = checkbox.checked;

      checkbox
        .closest(".jb-grid-row")
        ?.classList.toggle(
          "jb-row-done",
          checkbox.checked
        );

      checkbox
        .closest(".jb-mobile-specimen-card")
        ?.classList.toggle(
          "jb-row-done",
          checkbox.checked
        );

      autosave();
    }
  });

  document.addEventListener("click", event => {
    const button = event.target.closest("[data-action]");

    if (!button) {
      return;
    }

    const action = button.dataset.action;
    const eventId = button.dataset.eventId;
    const specimenId = button.dataset.specimenId;
    const column = button.dataset.col;

    const handlers = {
      "download-empty": downloadEmptyWorkbook,
      "save-draft": saveDraft,
      "load-draft": loadDraft,
      "export-filled": exportFilledWorkbook,
      "export-fieldbook-pdf": exportFieldbookPDF,
      "add-event-top": () => addEvent("start"),
      "add-event-after-last": () => addEvent("end"),
      "add-specimen": () => addSpecimen(eventId),
      "duplicate-last-specimen": () => duplicateLastSpecimen(eventId),
      "remove-specimen": () => removeSpecimen(eventId, specimenId),
      "remove-event": () => removeEvent(eventId),
      "repeat-header": () => repeatHeader(eventId),
      "duplicate-event-header": () => duplicateEventHeader(eventId),
      "sequence-numbers": () => sequenceNumbers(eventId),

      "toggle-event": () => {
        const eventItem = findEvent(eventId);

        if (eventItem) {
          eventItem.collapsed = !eventItem.collapsed;
        }

        render();
        autosave();
      },

      "toggle-specimen-details": () => {
        const eventItem = findEvent(eventId);

        const specimen = findSpecimen(
          eventItem,
          specimenId
        );

        if (specimen) {
          specimen.detailsOpen = !specimen.detailsOpen;
        }

        render();
        autosave();
      },

      "confirm-taxon": () => {
        const eventItem = findEvent(eventId);

        const specimen = findSpecimen(
          eventItem,
          specimenId
        );

        if (specimen) {
          specimen.done = true;
          specimen.detailsOpen = false;

          render();
          autosave();

          toast(
            t(
              "toast.taxonConfirmed",
              {},
              "Táxon confirmado."
            )
          );
        }
      },

      "widen-col": () => setWidth(column, 32),
      "narrow-col": () => setWidth(column, -32)
    };

    if (handlers[action]) {
      handlers[action]();
    }
  });

  function positionTooltip(target) {
    if (!tooltipEl || !target) {
      return;
    }

    const text =
      target.getAttribute("data-tip") ||
      target.getAttribute("aria-label") ||
      "";

    if (!text) {
      return;
    }

    tooltipEl.textContent = text;
    tooltipEl.classList.add("is-visible");

    const rect = target.getBoundingClientRect();
    const tt = tooltipEl.getBoundingClientRect();
    const margin = 12;

    let left =
      rect.left +
      rect.width / 2 -
      tt.width / 2;

    left = Math.max(
      margin,
      Math.min(
        window.innerWidth - tt.width - margin,
        left
      )
    );

    let top = rect.bottom + 10;

    if (
      top +
      tt.height +
      margin >
      window.innerHeight
    ) {
      top =
        rect.top -
        tt.height -
        10;
    }

    top = Math.max(margin, top);

    tooltipEl.style.left = `${left}px`;
    tooltipEl.style.top = `${top}px`;
  }

  function hideTooltip() {
    tooltipEl.classList.remove("is-visible");
  }

  document.addEventListener("mouseover", event => {
    const help = event.target.closest(".jb-help");
    if (help) positionTooltip(help);
  });

  document.addEventListener("mouseout", event => {
    const help = event.target.closest(".jb-help");
    if (help) hideTooltip();
  });

  document.addEventListener("focusin", event => {
    const help = event.target.closest(".jb-help");
    if (help) positionTooltip(help);
  });

  document.addEventListener("focusout", event => {
    const help = event.target.closest(".jb-help");
    if (help) hideTooltip();
  });

  window.addEventListener("resize", hideTooltip);
  window.addEventListener("scroll", hideTooltip, true);

  render();
})();
