package excel.engine.o2w.extractor;

import excel.engine.model.meta.FieldMeta;
import excel.engine.o2w.composer.WorkbookComposeException;
import excel.engine.util.FieldUtils;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * using {@link BeanUtils#getProperty(Object, String)}, this use to fallback
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class BeanUtilValueExtractor implements ValueExtractor {

  private static final Logger LOGGER = LoggerFactory.getLogger(BeanUtilValueExtractor.class);

  @Override
  public String getStringValue(Object data, FieldMeta fieldMeta) {
    try {
      return BeanUtils.getProperty(data, FieldUtils.detectRealField(fieldMeta.getName()));
    } catch (NestedNullException e) {
      LOGGER.trace(ExceptionUtils.getStackTrace(e));
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookComposeException(e);
    }
  }

}
