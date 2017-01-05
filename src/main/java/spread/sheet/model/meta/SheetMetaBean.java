package spread.sheet.model.meta;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2016/12/27.
 */
public class SheetMetaBean implements SheetMeta {

  private String sheetName;

  private int dataStartRowIndex;

  private List<FieldMeta> fieldMetas = new ArrayList<>();

  public SheetMetaBean(int dataStartRowIndex) {
    this.dataStartRowIndex = dataStartRowIndex;
  }

  public SheetMetaBean(String sheetName, int dataStartRowIndex) {
    this.sheetName = sheetName;
    this.dataStartRowIndex = dataStartRowIndex;
  }

  public String getSheetName() {
    return sheetName;
  }

  @Override
  public int getDataStartRowIndex() {
    return dataStartRowIndex;
  }

  @Override
  public List<FieldMeta> getFieldMetas() {
    Collections.sort(fieldMetas);
    return fieldMetas;
  }

  @Override
  public FieldMeta getFieldMeta(String field) {
    for (FieldMeta fieldMeta : fieldMetas) {
      if (StringUtils.equals(fieldMeta.getName(), field)) {
        return fieldMeta;
      }
    }
    return null;
  }

  @Override
  public void addFieldMeta(FieldMeta fieldMeta) {
    ((FieldMetaBean) fieldMeta).setSheetMeta(this);
    this.fieldMetas.add(fieldMeta);
  }
}
