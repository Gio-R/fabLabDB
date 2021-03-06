--
-- PostgreSQL database dump
--

-- Dumped from database version 11.4
-- Dumped by pg_dump version 11.4

-- Started on 2019-10-06 10:54:41

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 2 (class 3079 OID 16729)
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- TOC entry 2941 (class 0 OID 0)
-- Dependencies: 2
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 198 (class 1259 OID 16456)
-- Name: classi_di_materiali; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.classi_di_materiali (
    codice_classe character(2) NOT NULL,
    nome character varying(30) NOT NULL
);


ALTER TABLE public.classi_di_materiali OWNER TO postgres;

--
-- TOC entry 205 (class 1259 OID 16643)
-- Name: composizioni; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.composizioni (
    codice_lavorazione character(20) NOT NULL,
    codice_intaglio integer NOT NULL
);


ALTER TABLE public.composizioni OWNER TO postgres;

--
-- TOC entry 207 (class 1259 OID 16663)
-- Name: filamenti; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.filamenti (
    codice_filamento character(7) NOT NULL,
    codice_plastica character(3) NOT NULL,
    marca character varying(30) NOT NULL,
    colore character varying(30) NOT NULL
);


ALTER TABLE public.filamenti OWNER TO postgres;

--
-- TOC entry 203 (class 1259 OID 16593)
-- Name: intagli; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.intagli (
    codice_intaglio integer NOT NULL,
    data_richiesta date NOT NULL,
    data_consegna date,
    tempo double precision,
    costo_materiali numeric(6,2),
    costo_totale numeric(6,2),
    cf_richiedente character(16) NOT NULL,
    cf_incaricato character(16),
    descrizione character varying(400) NOT NULL,
    CONSTRAINT date_intaglio CHECK ((data_richiesta <= data_consegna)),
    CONSTRAINT tempo_intaglio CHECK ((tempo > (0)::double precision))
);


ALTER TABLE public.intagli OWNER TO postgres;

--
-- TOC entry 210 (class 1259 OID 16722)
-- Name: intagli_codice_intaglio_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.intagli ALTER COLUMN codice_intaglio ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.intagli_codice_intaglio_seq
    START WITH 0
    INCREMENT BY 1
    MINVALUE 0
    NO MAXVALUE
    CACHE 1
);


--
-- TOC entry 202 (class 1259 OID 16561)
-- Name: lavorazioni; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.lavorazioni (
    codice_tipo character(3) NOT NULL,
    codice_lavorazione character(20) NOT NULL,
    codice_materiale character(8) NOT NULL,
    potenza_massima integer NOT NULL,
    potenza_minima integer NOT NULL,
    velocita integer NOT NULL,
    descrizione character varying(400) NOT NULL
);


ALTER TABLE public.lavorazioni OWNER TO postgres;

--
-- TOC entry 199 (class 1259 OID 16461)
-- Name: materiali; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.materiali (
    codice_classe character(2) NOT NULL,
    codice_materiale character(8) NOT NULL,
    nome character varying(30) NOT NULL,
    spessore double precision NOT NULL,
    descrizione character varying(400) NOT NULL
);


ALTER TABLE public.materiali OWNER TO postgres;

--
-- TOC entry 200 (class 1259 OID 16496)
-- Name: persone; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.persone (
    cf character(16) NOT NULL,
    nome character varying(30) NOT NULL,
    cognome character varying(30) NOT NULL,
    socio boolean NOT NULL,
    operatore_intagliatrice boolean NOT NULL,
    operatore_stampante boolean NOT NULL,
    spesa_totale numeric(6,2) NOT NULL,
    CONSTRAINT abilitazioni CHECK (((socio = true) OR ((operatore_intagliatrice = false) AND (operatore_stampante = false))))
);


ALTER TABLE public.persone OWNER TO postgres;

--
-- TOC entry 206 (class 1259 OID 16658)
-- Name: plastiche; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.plastiche (
    codice_plastica character(3) NOT NULL,
    nome character varying(100) NOT NULL,
    descrizione character varying(400) NOT NULL
);


ALTER TABLE public.plastiche OWNER TO postgres;

--
-- TOC entry 212 (class 1259 OID 16791)
-- Name: sessioni; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.sessioni (
    ora_creazione timestamp(6) with time zone NOT NULL,
    id_sessione integer NOT NULL,
    utente character varying(30) NOT NULL
);


ALTER TABLE public.sessioni OWNER TO postgres;

--
-- TOC entry 213 (class 1259 OID 16796)
-- Name: sessioni_id_sessione_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.sessioni ALTER COLUMN id_sessione ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.sessioni_id_sessione_seq
    START WITH 0
    INCREMENT BY 1
    MINVALUE 0
    NO MAXVALUE
    CACHE 1
);


--
-- TOC entry 197 (class 1259 OID 16451)
-- Name: stampanti; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.stampanti (
    codice_stampante character(3) NOT NULL,
    marca character varying(30) NOT NULL,
    modello character varying(30) NOT NULL,
    descrizione character varying(400) NOT NULL
);


ALTER TABLE public.stampanti OWNER TO postgres;

--
-- TOC entry 204 (class 1259 OID 16616)
-- Name: stampe; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.stampe (
    codice_stampa integer NOT NULL,
    data_richiesta date NOT NULL,
    data_consegna date,
    tempo double precision,
    costo_materiali numeric(6,2),
    costo_totale numeric(6,2),
    descrizione character varying(400) NOT NULL,
    cf_richiedente character(16) NOT NULL,
    cf_incaricato character(16),
    codice_stampante character(3),
    CONSTRAINT date_stampe CHECK ((data_richiesta <= data_consegna)),
    CONSTRAINT tempo_stampe CHECK ((tempo > (0)::double precision))
);


ALTER TABLE public.stampe OWNER TO postgres;

--
-- TOC entry 209 (class 1259 OID 16720)
-- Name: stampe_codice_stampa_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.stampe ALTER COLUMN codice_stampa ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.stampe_codice_stampa_seq
    START WITH 0
    INCREMENT BY 1
    MINVALUE 0
    NO MAXVALUE
    CACHE 1
);


--
-- TOC entry 201 (class 1259 OID 16503)
-- Name: tipi; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tipi (
    codice_tipo character(3) NOT NULL,
    nome character varying(30) NOT NULL,
    descrizione character varying(400) NOT NULL
);


ALTER TABLE public.tipi OWNER TO postgres;

--
-- TOC entry 208 (class 1259 OID 16675)
-- Name: usi; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.usi (
    codice_stampa integer NOT NULL,
    codice_filamento character(7) NOT NULL
);


ALTER TABLE public.usi OWNER TO postgres;

--
-- TOC entry 211 (class 1259 OID 16786)
-- Name: utenti; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.utenti (
    username character varying(30) NOT NULL,
    hash bytea NOT NULL,
    admin boolean DEFAULT false NOT NULL
);


ALTER TABLE public.utenti OWNER TO postgres;

--
-- TOC entry 2798 (class 2606 OID 16790)
-- Name: utenti admins_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.utenti
    ADD CONSTRAINT admins_pkey PRIMARY KEY (username);


--
-- TOC entry 2772 (class 2606 OID 16509)
-- Name: classi_di_materiali classi di materiali_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.classi_di_materiali
    ADD CONSTRAINT "classi di materiali_pkey" PRIMARY KEY (codice_classe);


--
-- TOC entry 2788 (class 2606 OID 16878)
-- Name: composizioni composizioni_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.composizioni
    ADD CONSTRAINT composizioni_pkey PRIMARY KEY (codice_lavorazione, codice_intaglio);


--
-- TOC entry 2760 (class 2606 OID 16714)
-- Name: intagli consegna_intaglio; Type: CHECK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE public.intagli
    ADD CONSTRAINT consegna_intaglio CHECK (((data_consegna IS NULL) OR ((data_consegna IS NOT NULL) AND (costo_totale IS NOT NULL) AND (costo_materiali IS NOT NULL) AND (tempo IS NOT NULL) AND (cf_incaricato IS NOT NULL)))) NOT VALID;


--
-- TOC entry 2764 (class 2606 OID 16702)
-- Name: stampe consegna_stampe; Type: CHECK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE public.stampe
    ADD CONSTRAINT consegna_stampe CHECK (((data_consegna IS NULL) OR ((data_consegna IS NOT NULL) AND (costo_totale IS NOT NULL) AND (costo_materiali IS NOT NULL) AND (tempo IS NOT NULL) AND (cf_incaricato IS NOT NULL) AND (codice_stampante IS NOT NULL)))) NOT VALID;


--
-- TOC entry 2761 (class 2606 OID 16719)
-- Name: intagli costi_intaglio; Type: CHECK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE public.intagli
    ADD CONSTRAINT costi_intaglio CHECK (((costo_totale >= costo_materiali) AND (costo_materiali >= (0)::numeric))) NOT VALID;


--
-- TOC entry 2765 (class 2606 OID 16707)
-- Name: stampe costi_stampe; Type: CHECK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE public.stampe
    ADD CONSTRAINT costi_stampe CHECK (((costo_totale >= costo_materiali) AND (costo_materiali >= (0)::numeric))) NOT VALID;


--
-- TOC entry 2792 (class 2606 OID 16817)
-- Name: filamenti filamenti_codice_plastica_marca_colore_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.filamenti
    ADD CONSTRAINT filamenti_codice_plastica_marca_colore_key UNIQUE (codice_plastica, marca, colore);


--
-- TOC entry 2794 (class 2606 OID 16667)
-- Name: filamenti filamenti_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.filamenti
    ADD CONSTRAINT filamenti_pkey PRIMARY KEY (codice_filamento);


--
-- TOC entry 2784 (class 2606 OID 16601)
-- Name: intagli intagli_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.intagli
    ADD CONSTRAINT intagli_pkey PRIMARY KEY (codice_intaglio);


--
-- TOC entry 2757 (class 2606 OID 16584)
-- Name: lavorazioni intervallo_potenza_max; Type: CHECK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE public.lavorazioni
    ADD CONSTRAINT intervallo_potenza_max CHECK (((potenza_massima >= 0) AND (potenza_massima < 300))) NOT VALID;


--
-- TOC entry 2758 (class 2606 OID 16585)
-- Name: lavorazioni intervallo_potenza_min; Type: CHECK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE public.lavorazioni
    ADD CONSTRAINT intervallo_potenza_min CHECK (((potenza_minima >= 0) AND (potenza_massima < 300))) NOT VALID;


--
-- TOC entry 2759 (class 2606 OID 16586)
-- Name: lavorazioni intervallo_velocita; Type: CHECK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE public.lavorazioni
    ADD CONSTRAINT intervallo_velocita CHECK (((velocita > 0) AND (velocita < 300))) NOT VALID;


--
-- TOC entry 2780 (class 2606 OID 16866)
-- Name: lavorazioni lavorazioni_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.lavorazioni
    ADD CONSTRAINT lavorazioni_pkey PRIMARY KEY (codice_lavorazione);


--
-- TOC entry 2782 (class 2606 OID 16854)
-- Name: lavorazioni lavorazioni_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.lavorazioni
    ADD CONSTRAINT lavorazioni_unique UNIQUE (codice_materiale, potenza_minima, potenza_massima, codice_tipo, velocita);


--
-- TOC entry 2774 (class 2606 OID 16843)
-- Name: materiali materiali_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.materiali
    ADD CONSTRAINT materiali_pkey PRIMARY KEY (codice_materiale);


--
-- TOC entry 2776 (class 2606 OID 16502)
-- Name: persone persone_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.persone
    ADD CONSTRAINT persone_pkey PRIMARY KEY (cf);


--
-- TOC entry 2790 (class 2606 OID 16662)
-- Name: plastiche plastiche_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.plastiche
    ADD CONSTRAINT plastiche_pkey PRIMARY KEY (codice_plastica);


--
-- TOC entry 2800 (class 2606 OID 16799)
-- Name: sessioni sessioni_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.sessioni
    ADD CONSTRAINT sessioni_pkey PRIMARY KEY (id_sessione);


--
-- TOC entry 2756 (class 2606 OID 16694)
-- Name: persone spesa; Type: CHECK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE public.persone
    ADD CONSTRAINT spesa CHECK ((spesa_totale >= (0)::numeric)) NOT VALID;


--
-- TOC entry 2754 (class 2606 OID 16560)
-- Name: materiali spessore_positivo; Type: CHECK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE public.materiali
    ADD CONSTRAINT spessore_positivo CHECK ((spessore > (0)::double precision)) NOT VALID;


--
-- TOC entry 2770 (class 2606 OID 16455)
-- Name: stampanti stampanti_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.stampanti
    ADD CONSTRAINT stampanti_pkey PRIMARY KEY (codice_stampante);


--
-- TOC entry 2786 (class 2606 OID 16624)
-- Name: stampe stampe_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.stampe
    ADD CONSTRAINT stampe_pkey PRIMARY KEY (codice_stampa);


--
-- TOC entry 2778 (class 2606 OID 16890)
-- Name: tipi tipi_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tipi
    ADD CONSTRAINT tipi_pkey PRIMARY KEY (codice_tipo);


--
-- TOC entry 2796 (class 2606 OID 16679)
-- Name: usi usi_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.usi
    ADD CONSTRAINT usi_pkey PRIMARY KEY (codice_stampa, codice_filamento);


--
-- TOC entry 2801 (class 2606 OID 16510)
-- Name: materiali classe_materiale; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.materiali
    ADD CONSTRAINT classe_materiale FOREIGN KEY (codice_classe) REFERENCES public.classi_di_materiali(codice_classe) ON DELETE CASCADE;


--
-- TOC entry 2810 (class 2606 OID 16653)
-- Name: composizioni composizioni_codice_intaglio_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.composizioni
    ADD CONSTRAINT composizioni_codice_intaglio_fkey FOREIGN KEY (codice_intaglio) REFERENCES public.intagli(codice_intaglio) ON DELETE CASCADE;


--
-- TOC entry 2809 (class 2606 OID 16879)
-- Name: composizioni composizioni_codice_lavorazione_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.composizioni
    ADD CONSTRAINT composizioni_codice_lavorazione_fkey FOREIGN KEY (codice_lavorazione) REFERENCES public.lavorazioni(codice_lavorazione) ON DELETE CASCADE;


--
-- TOC entry 2811 (class 2606 OID 16818)
-- Name: filamenti filamenti_codice_plastica_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.filamenti
    ADD CONSTRAINT filamenti_codice_plastica_fkey FOREIGN KEY (codice_plastica) REFERENCES public.plastiche(codice_plastica) ON DELETE CASCADE;


--
-- TOC entry 2805 (class 2606 OID 16607)
-- Name: intagli incaricato_intaglio; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.intagli
    ADD CONSTRAINT incaricato_intaglio FOREIGN KEY (cf_incaricato) REFERENCES public.persone(cf);


--
-- TOC entry 2807 (class 2606 OID 16630)
-- Name: stampe incaricato_stampa; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.stampe
    ADD CONSTRAINT incaricato_stampa FOREIGN KEY (cf_incaricato) REFERENCES public.persone(cf);


--
-- TOC entry 2802 (class 2606 OID 16855)
-- Name: lavorazioni materiale_lavorazione_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.lavorazioni
    ADD CONSTRAINT materiale_lavorazione_fkey FOREIGN KEY (codice_materiale) REFERENCES public.materiali(codice_materiale) ON DELETE CASCADE;


--
-- TOC entry 2804 (class 2606 OID 16602)
-- Name: intagli richiedente_intaglio; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.intagli
    ADD CONSTRAINT richiedente_intaglio FOREIGN KEY (cf_richiedente) REFERENCES public.persone(cf);


--
-- TOC entry 2806 (class 2606 OID 16625)
-- Name: stampe richiedente_stampa; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.stampe
    ADD CONSTRAINT richiedente_stampa FOREIGN KEY (cf_richiedente) REFERENCES public.persone(cf);


--
-- TOC entry 2814 (class 2606 OID 16800)
-- Name: sessioni sessioni_admin_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.sessioni
    ADD CONSTRAINT sessioni_admin_fkey FOREIGN KEY (utente) REFERENCES public.utenti(username);


--
-- TOC entry 2808 (class 2606 OID 16635)
-- Name: stampe stampante; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.stampe
    ADD CONSTRAINT stampante FOREIGN KEY (codice_stampante) REFERENCES public.stampanti(codice_stampante);


--
-- TOC entry 2803 (class 2606 OID 16891)
-- Name: lavorazioni tipo_lavorazione_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.lavorazioni
    ADD CONSTRAINT tipo_lavorazione_fkey FOREIGN KEY (codice_tipo) REFERENCES public.tipi(codice_tipo) ON DELETE CASCADE;


--
-- TOC entry 2813 (class 2606 OID 16685)
-- Name: usi usi_codice_filamento_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.usi
    ADD CONSTRAINT usi_codice_filamento_fkey FOREIGN KEY (codice_filamento) REFERENCES public.filamenti(codice_filamento);


--
-- TOC entry 2812 (class 2606 OID 16680)
-- Name: usi usi_codice_stampa_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.usi
    ADD CONSTRAINT usi_codice_stampa_fkey FOREIGN KEY (codice_stampa) REFERENCES public.stampe(codice_stampa);


-- Completed on 2019-10-06 10:54:41

--
-- PostgreSQL database dump complete
--

