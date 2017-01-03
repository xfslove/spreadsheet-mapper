package extensible.sheet.o2w.extractor;

import extensible.sheet.model.meta.FieldMeta;

/**
 * field value extractor adapter, easy implements customer value extractor extends this.
 * <p>
 * Created by hanwen on 5/3/16.
 */
public abstract class FieldValueExtractorAdapter implements FieldValueExtractor {

  private String matchField;

  public FieldValueExtractorAdapter(String matchField) {
    this.matchField = matchField;
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  @Override
  public abstract String getStringValue(Object data, FieldMeta fieldMeta);
}
