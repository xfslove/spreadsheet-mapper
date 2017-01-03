package extensible.sheet.model.meta;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 2016/12/30.
 */
public class FieldMetaBean implements FieldMeta {

  private String name;

  private int columnIndex;

  private List<HeaderMeta> headerMetas = new ArrayList<>();

  private SheetMeta sheetMeta;

  public FieldMetaBean(String name, int columnIndex) {
    this.name = name;
    this.columnIndex = columnIndex;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public int getColumnIndex() {
    return columnIndex;
  }

  @Override
  public List<HeaderMeta> getHeaderMetas() {
    return headerMetas;
  }

  @Override
  public HeaderMeta getHeaderMeta(int rowIndex) {
    for (HeaderMeta headerMeta : headerMetas) {
      if (headerMeta.getRowIndex() == rowIndex) {
        return headerMeta;
      }
    }
    return null;
  }

  @Override
  public void addHeaderMeta(HeaderMeta headerMeta) {
    ((HeaderMetaBean) headerMeta).setFieldMeta(this);
    headerMetas.add(headerMeta);
  }

  @Override
  public SheetMeta getSheetMeta() {
    return sheetMeta;
  }

  void setSheetMeta(SheetMeta sheetMeta) {
    this.sheetMeta = sheetMeta;
  }
}

