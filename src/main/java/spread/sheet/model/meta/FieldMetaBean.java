package spread.sheet.model.meta;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.CompareToBuilder;

import java.util.ArrayList;
import java.util.Collections;
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
    if (StringUtils.isBlank(name)) {
      throw new IllegalArgumentException("name can not be null");
    }
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
    Collections.sort(headerMetas);
    return headerMetas;
  }

  @Override
  public HeaderMeta getHeaderMeta(int rowIndex) {
    // maybe some row index not has field meta
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

  @Override
  public int compareTo(FieldMeta o) {
    return new CompareToBuilder().append(columnIndex, o.getColumnIndex()).toComparison();
  }
}

