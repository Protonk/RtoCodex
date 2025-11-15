<!-- event-narratives/AGENTS.md clarifies how to record and maintain narratives. -->
# Event Narratives · AGENTS guide

Event narratives capture concrete observations tied to a specific debugging or diagnostic session. To keep them faithful to the original context:

1. **Only edit the narrative you are currently recounting.** Do not refactor, rename, or “tidy up” unrelated narratives unless the user explicitly asks. Historical accounts are meant to stay frozen except when you are the agent relaying the event itself.
2. **When adding a new narrative**, follow the format in root `AGENTS.md` (HTML comment header, title, Summary/Sequence/Locus sections). Save it as `event_MM_DD_ShortName.md`.
3. **Revisions to an existing narrative** should only extend the “Follow-up” section or append a new dated summary when the same event resurfaces; never rewrite the original observations without instruction.
4. **Link to artifacts** (logs, combo names) rather than paraphrasing large outputs; future agents should be able to reproduce the situation by following your breadcrumb trail.
